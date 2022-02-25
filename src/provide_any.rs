//! `provide_any`, a generic API for objects to provide type-based access to data.
//!
//! `provide_any` supports generic, type-driven access to data and a mechanism for intermediate
//! implementers to provide such data. The key parts of `provide_any`'s interface are the `Provider`
//! trait for objects which can provide data, and the `request_by_type_tag` function for requesting
//! data from an object which implements `Provider`. Note that end users should not call
//! `request_by_type_tag` directly, it is a helper function for intermediate implementers to use.
//!
//! Typically, a data provider is a trait object of a trait which extends `Provider`. A user will
//! request data from the trait object by specifying the type or a type tag (a type tag is a type
//! used only as a type parameter to identify the type which the user wants to receive).
//!
//! ```no-build
//! // Request a &String from obj.
//! let _ = obj.request_ref::<String>().unwrap();
//! // Request a &String from obj using a type tag.
//! let _ = obj.request_by_type_tag::<tags::Ref<String>>().unwrap();
//! ```
//!
//! In this example obj is a trait object of some trait which has `request_ref` and
//! `request_by_type_tag` methods, both are implemented using [`provide_any::request_by_type_tag`].
//! That trait will also extend `Provider`.
//!
//! ## Data flow
//!
//! * A user requests an object, which is delegated to `request_by_type_tag`
//! * `request_by_type_tag` creates a `Requisition` object and passes it to `Provider::provide`
//! * The object provider's implementation of `Provider::provide` tries providing values of
//!   different types using `Requisition::provide_*`. If the type tag matches the type requested by
//!   the user, it will be stored in the `Requisition` object.
//! * `request_by_type_tag` unpacks the `Requisition` object and returns any stored value to the user.

use core::any::TypeId;

/// Trait implemented by a type which can dynamically provide tagged values.
pub trait Provider {
    /// Object providers should implement this method to provide *all* values they are able to
    /// provide using `req`.
    fn provide<'a>(&'a self, req: &mut Requisition<'a>);
    fn provide_mut<'a>(&'a mut self, _req: &mut Requisition<'a>) {}
}

/// Request a specific value by a given tag from the `Provider`.
pub fn request_by_type_tag<'a, I>(provider: &'a dyn Provider) -> Option<I::Type>
where
    I: TypeTag<'a>,
{
    let mut req: ConcreteRequisition<'a, I> = RequisitionImpl {
        tagged: TagValue(None),
    };
    provider.provide(&mut req);
    req.tagged.0
}

pub fn request_mut_by_type_tag<'a, I>(provider: &'a mut dyn Provider) -> Option<I::Type>
where
    I: TypeTag<'a>,
{
    let mut req: ConcreteRequisition<'a, I> = RequisitionImpl {
        tagged: TagValue(None),
    };
    provider.provide_mut(&mut req);
    req.tagged.0
}

/// This trait is implemented by specific `TypeTag` types in order to allow
/// describing a type which can be requested for a given lifetime `'a`.
///
/// A few example implementations for type-driven `TypeTag`s can be found in the
/// [`tag`] module, although crates may also implement their own tags for more
/// complex types with internal lifetimes.
pub trait TypeTag<'a>: Sized + 'static {
    /// The type of values which may be tagged by this `TypeTag` for the given
    /// lifetime.
    type Type: 'a;
}

pub mod tags {
    //! Type tags are used to identify a type using a separate value. This module includes type tags
    //! for some very common types.
    //!
    //! Many users of [`provide_any`] will not need to use type tags at all. But if you want to use
    //! [`provide_any`] with more complex types (typically those including lifetime parameters), you
    //! will need to write your own tags.

    use super::TypeTag;
    use core::marker::PhantomData;

    /// Type-based `TypeTag` for `&'a T` types.
    pub struct Ref<T: ?Sized + 'static>(PhantomData<T>);

    impl<'a, T: ?Sized + 'static> TypeTag<'a> for Ref<T> {
        type Type = &'a T;
    }

    /// Type-based `TypeTag` for `&'a mut T` types.
    pub struct RefMut<T: ?Sized + 'static>(PhantomData<T>);

    impl<'a, T: ?Sized + 'static> TypeTag<'a> for RefMut<T> {
        type Type = &'a mut T;
    }

    /// Type-based `TypeTag` for static `T` types.
    pub struct Value<T: 'static>(PhantomData<T>);

    impl<'a, T: 'static> TypeTag<'a> for Value<T> {
        type Type = T;
    }

    /// Tag combinator to wrap the given tag's value in an `Option<T>`
    pub struct OptionTag<I>(PhantomData<I>);

    impl<'a, I: TypeTag<'a>> TypeTag<'a> for OptionTag<I> {
        type Type = Option<I::Type>;
    }

    /// Tag combinator to wrap the given tag's value in an `Result<T, E>`
    pub struct ResultTag<I, E>(PhantomData<I>, PhantomData<E>);

    impl<'a, I: TypeTag<'a>, E: TypeTag<'a>> TypeTag<'a> for ResultTag<I, E> {
        type Type = Result<I::Type, E::Type>;
    }
}

mod tags2 {
    use core::marker::PhantomData;

    pub trait Type<'a>: Sized + 'static {
        type Reified: 'a;
    }

    pub trait TypeComponent<'a>: Sized + 'static {
        type Reified: 'a + ?Sized;
    }

    impl<'a, T: Type<'a>> TypeComponent<'a> for T {
        type Reified = T::Reified;
    }


    /// Type-based `TypeTag` for static `T` types.
    pub struct Value<T: 'static>(PhantomData<T>);

    impl<'a, T: 'static> Type<'a> for Value<T> {
        type Reified = T;
    }

    /// Type-based `TypeTag` for static `T` types.
    pub struct MaybeSizedValue<T: ?Sized + 'static>(PhantomData<T>);

    impl<'a, T: ?Sized + 'static> TypeComponent<'a> for MaybeSizedValue<T> {
        type Reified = T;
    }


    /// Type-based `TypeTag` for `&'a T` types.
    pub struct Ref<I>(PhantomData<I>);

    impl<'a, I: TypeComponent<'a>> Type<'a> for Ref<I> {
        type Reified = &'a I::Reified;
    }

    /// Type-based `TypeTag` for `&'a mut T` types.
    pub struct RefMut<I>(PhantomData<I>);

    impl<'a, I: TypeComponent<'a>> Type<'a> for RefMut<I> {
        type Reified = &'a mut I::Reified;
    }

    /// Tag combinator to wrap the given tag's value in an `Option<T>`
    pub struct Option<I>(PhantomData<I>);

    impl<'a, I: Type<'a>> Type<'a> for Option<I> {
        type Reified = Option<I::Reified>;
    }

    /// Tag combinator to wrap the given tag's value in an `Result<T, E>`
    pub struct Result<I, E>(PhantomData<I>, PhantomData<E>);

    impl<'a, I: Type<'a>, E: Type<'a>> Type<'a> for Result<I, E> {
        type Reified = Result<I::Reified, E::Reified>;
    }
}

/// A helper object for providing objects by type.
///
/// An object provider provides values by calling this type's provide methods.
pub type Requisition<'a> = RequisitionImpl<dyn Tagged<'a> + 'a>;

impl<'a> Requisition<'a> {
    /// Provide a value or other type with only static lifetimes.
    pub fn provide_value<T, F>(&mut self, f: F) -> &mut Self
    where
        T: 'static,
        F: FnOnce() -> T,
    {
        self.provide_with::<tags::Value<T>, F>(f)
    }

    /// Provide a reference, note that the referee type must be bounded by `'static`, but may be unsized.
    pub fn provide_ref<T: ?Sized + 'static>(&mut self, value: &'a T) -> &mut Self {
        self.provide::<tags::Ref<T>>(value)
    }

    pub fn provide_mut<T, F>(&mut self, f: F) -> &mut Self
    where
        T: ?Sized + 'static,
        F: FnOnce() -> &'a mut T,
    {
        self.provide_with::<tags::RefMut<T>, F>(f)
    }

    /// Provide a value with the given `TypeTag`.
    pub fn provide<I>(&mut self, value: I::Type) -> &mut Self
    where
        I: TypeTag<'a>,
    {
        if let Some(res @ TagValue(None)) = self.tagged.downcast_mut::<tags::OptionTag<I>>() {
            res.0 = Some(value);
        }
        self
    }

    /// Provide a value with the given `TypeTag`, using a closure to prevent unnecessary work.
    pub fn provide_with<I, F>(&mut self, f: F) -> &mut Self
    where
        I: TypeTag<'a>,
        F: FnOnce() -> I::Type,
    {
        if let Some(res @ TagValue(None)) = self.tagged.downcast_mut::<tags::OptionTag<I>>() {
            res.0 = Some(f());
        }
        self
    }

    pub fn within<C>(&mut self, cx: C) -> DemandWithin<'a, '_, C> {
        DemandWithin {
            cx,
            demand: self,
        }
    }
}
pub struct DemandWithin<'a, 'b, C> {
    cx: C,
    demand: &'b mut Requisition<'a>,
}

impl<'a, C> DemandWithin<'a, '_, C> {
    pub fn provide<T: ?Sized + 'static, F: FnOnce(C) -> &'a mut T>(self, f: F) -> Self {
        // if let Some(res @ TagValue(None)) = self.demand.tagged.downcast_mut::<tags::OptionTag<tags::RefMut<T>>>() {
        //     res.0 = Some(f(self.cx));
        // }
        // self
        panic!()
    }
}

/// A concrete request for a tagged value. Can be coerced to `Requisition` to be
/// passed to provider methods.
type ConcreteRequisition<'a, I> = RequisitionImpl<TagValue<'a, tags::OptionTag<I>>>;

/// Implementation detail shared between `Requisition` and `ConcreteRequisition`.
///
/// Generally this value is used through the `Requisition` type as an `&mut
/// Requisition<'a>` out parameter, or constructed with the `ConcreteRequisition<'a, I>`
/// type alias.
#[repr(transparent)]
pub struct RequisitionImpl<T: ?Sized> {
    tagged: T,
}

/// Sealed trait representing a type-erased tagged object.
///
/// This trait is exclusively implemented by the `TagValue` type, and cannot be
/// implemented outside of this crate due to being sealed.
pub unsafe trait Tagged<'a>: 'a {
    /// The `TypeId` of the `TypeTag` this value was tagged with.
    fn tag_id(&self) -> TypeId;
}

/// A concrete tagged value for a given tag `I`.
///
/// This is the only type which implements the `Tagged` trait, and encodes
/// additional information about the specific `TypeTag` into the type. This allows
/// for multiple different tags to support overlapping value ranges, for
/// example, both the `Ref<str>` and `Value<&'static str>` tags can be used to
/// tag a value of type `&'static str`.
#[repr(transparent)]
struct TagValue<'a, I: TypeTag<'a>>(I::Type);

unsafe impl<'a, I> Tagged<'a> for TagValue<'a, I>
where
    I: TypeTag<'a>,
{
    fn tag_id(&self) -> TypeId {
        TypeId::of::<I>()
    }
}

impl<'a> dyn Tagged<'a> {
    /// Returns `true` if the dynamic type is tagged with `I`.
    #[inline]
    fn is<I>(&self) -> bool
    where
        I: TypeTag<'a>,
    {
        self.tag_id() == TypeId::of::<I>()
    }

    /// Returns some reference to the dynamic value if it is tagged with `I`,
    /// or `None` if it isn't.
    #[inline]
    fn downcast_mut<I>(&mut self) -> Option<&mut TagValue<'a, I>>
    where
        I: TypeTag<'a>,
    {
        if self.is::<I>() {
            // SAFETY: Just checked whether we're pointing to a
            // `TagValue<'a, I>`.
            unsafe { Some(&mut *(self as *mut Self as *mut TagValue<'a, I>)) }
        } else {
            None
        }
    }
}

mod v2 {
    use super::tags2;
    use core::any::TypeId;
    use core::marker::PhantomData;

    pub trait Provider {
        fn provide<'a>(&'a self, req: &mut Demand<'a>);
    }

    #[repr(transparent)]
    struct TaggedOption<'a, I: tags2::Type<'a>>(Option<I::Reified>);

    impl<'a, I: tags2::Type<'a>> TaggedOption<'a, I> {
        fn as_demand(&mut self) -> &mut Demand<'a> {
            unsafe { std::mem::transmute(self as &mut (dyn Erased<'a> + 'a)) }
        }
    }

    trait Erased<'a>: 'a {
        fn tag_id(&self) -> TypeId;
    }

    impl<'a, I: tags2::Type<'a>> Erased<'a> for TaggedOption<'a, I> {
        fn tag_id(&self) -> TypeId {
            TypeId::of::<I>()
        }
    }

    impl<'a> dyn Erased<'a> {
        /// Returns some reference to the dynamic value if it is tagged with `I`,
        /// or `None` if it isn't.
        #[inline]
        fn downcast_mut<I>(&mut self) -> Option<&mut TaggedOption<'a, I>>
        where
            I: tags2::Type<'a>,
        {
            if self.tag_id() == TypeId::of::<I>() {
                // SAFETY: Just checked whether we're pointing to a
                // `TagValue<'a, I>`.
                Some(unsafe { &mut *(self as *mut Self as *mut TaggedOption<'a, I>) })
            } else {
                None
            }
        }
    }

    #[repr(transparent)]
    pub struct Demand<'a>(dyn Erased<'a> + 'a);

    pub fn request_value<'a, T: 'static>(provider: &'a dyn Provider) -> Option<T> {
        request_by_type_tag::<'a, tags2::Value<T>>(provider)
    }

    pub fn request_ref<'a, T: ?Sized + 'static>(provider: &'a dyn Provider) -> Option<&'a T> {
        request_by_type_tag::<'a, tags2::Ref<tags2::MaybeSizedValue<T>>>(provider)
    }

    /// Request a specific value by a given tag from the `Provider`.
    fn request_by_type_tag<'a, I>(provider: &'a dyn Provider) -> Option<I::Reified>
    where
        I: tags2::Type<'a>,
    {
        let mut tagged = TaggedOption::<'a, I>(None);
        provider.provide(tagged.as_demand());
        req.0
    }

    impl<'a> Demand<'a> {
        /// Provide a value or other type with only static lifetimes.
        pub fn provide_value<T, F>(&mut self, f: F) -> &mut Self
        where
            T: 'static,
            F: FnOnce() -> T,
        {
            self.provide_with::<tags2::Value<T>, F>(f)
        }

        /// Provide a reference, note that the referee type must be bounded by `'static`, but may be unsized.
        pub fn provide_ref<T: ?Sized + 'static>(&mut self, value: &'a T) -> &mut Self {
            self.provide::<tags2::Ref<tags2::MaybeSizedValue<T>>>(value)
        }

        /// Provide a value with the given `TypeTag`.
        fn provide<I>(&mut self, value: I::Reified) -> &mut Self
        where
            I: tags2::Type<'a>,
        {
            if let Some(res @ TaggedOption(None)) = self.0.downcast_mut::<I>() {
                res.0 = Some(value);
            }
            self
        }

        /// Provide a value with the given `TypeTag`, using a closure to prevent unnecessary work.
        fn provide_with<I, F>(&mut self, f: F) -> &mut Self
        where
            I: tags2::Type<'a>,
            F: FnOnce() -> I::Reified,
        {
            if let Some(res @ TaggedOption(None)) = self.0.downcast_mut::<I>() {
                res.0 = Some(f());
            }
            self
        }
    }
}
