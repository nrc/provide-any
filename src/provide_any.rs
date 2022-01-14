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
    fn provide<'a>(&'a self, req: &mut Requisition<'a, '_>);
}

/// Request a specific value by a given tag from the `Provider`.
pub fn request_by_type_tag<'a, I>(provider: &'a dyn Provider) -> Option<I::Type>
where
    I: TypeTag<'a>,
{
    let mut req: ConcreteRequisition<'a, I> = RequisitionImpl {
        tagged: TagValue(None),
    };
    provider.provide(&mut Requisition(&mut req));
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
    pub struct Optional<I>(PhantomData<I>);

    impl<'a, I: TypeTag<'a>> TypeTag<'a> for Optional<I> {
        type Type = Option<I::Type>;
    }

    /// Tag combinator to wrap the given tag's value in an `Result<T, E>`
    pub struct ResultTag<I, E>(PhantomData<I>, PhantomData<E>);

    impl<'a, I: TypeTag<'a>, E: TypeTag<'a>> TypeTag<'a> for ResultTag<I, E> {
        type Type = Result<I::Type, E::Type>;
    }
}

/// A helper object for providing objects by type.
///
/// An object provider provides values by calling this type's provide methods.
pub struct Requisition<'a, 'b>(&'b mut RequisitionImpl<dyn Tagged<'a> + 'a>);
pub struct SendRequisition<'a, 'b>(&'b mut RequisitionImpl<dyn Tagged<'a> + 'a + Send>);
// TODO are these useful?
pub struct SyncRequisition<'a, 'b>(&'b mut RequisitionImpl<dyn Tagged<'a> + 'a + Sync>);
pub struct SendSyncRequisition<'a, 'b>(&'b mut RequisitionImpl<dyn Tagged<'a> + 'a + Send + Sync>);

// Ignore this module; it is just used to check at compile time that the concurrent flavours of
// `Requisition` satisfy the trait bounds that they should.
#[allow(dead_code)]
#[allow(unused_variables)]
mod auto_trait_assert {
    use super::*;

    fn assert_send(x: impl Send) {}
    fn assert_sync(x: impl Sync) {}
    fn req_auto<'a, 'b>(r: Requisition<'a, 'b>) {
        // assert_send(r);
        // assert_sync(r);
    }
    fn send_req_auto<'a, 'b>(r: SendRequisition<'a, 'b>) {
        assert_send(r);
        // assert_sync(r);
    }
    fn sync_req_auto<'a, 'b>(r: SyncRequisition<'a, 'b>) {
        // assert_send(r);
        assert_sync(r);
    }
    fn send_sync_req_auto<'a, 'b>(
        r1: SendSyncRequisition<'a, 'b>,
        r2: SendSyncRequisition<'a, 'b>,
    ) {
        assert_send(r1);
        assert_sync(r2);
    }
}

macro_rules! req_methods {
    ($($T: ident),*) => {$(
        impl<'a, 'b> $T<'a, 'b> {
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

            /// Provide a value with the given `TypeTag`.
            pub fn provide<I>(&mut self, value: I::Type) -> &mut Self
            where
                I: TypeTag<'a>,
            {
                if let Some(res @ TagValue(None)) = self.0.tagged.downcast_mut::<tags::Optional<I>>() {
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
                if let Some(res @ TagValue(None)) = self.0.tagged.downcast_mut::<tags::Optional<I>>() {
                    res.0 = Some(f());
                }
                self
            }
        }
    )*};
}

req_methods!(
    Requisition,
    SendRequisition,
    SyncRequisition,
    SendSyncRequisition
);

/// A concrete request for a tagged value. Can be coerced to `Requisition` to be
/// passed to provider methods.
type ConcreteRequisition<'a, I> = RequisitionImpl<TagValue<'a, tags::Optional<I>>>;

/// Implementation detail shared between `Requisition` and `ConcreteRequisition`.
///
/// Generally this value is used through the `Requisition` type as an `&mut
/// Requisition<'a>` out parameter, or constructed with the `ConcreteRequisition<'a, I>`
/// type alias.
#[repr(transparent)]
struct RequisitionImpl<T: ?Sized> {
    tagged: T,
}

/// Sealed trait representing a type-erased tagged object.
///
/// This trait is exclusively implemented by the `TagValue` type, and cannot be
/// implemented outside of this crate due to being sealed.
unsafe trait Tagged<'a>: 'a {
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

macro_rules! tagged_methods {
    ($($T: ty),*) => {$(
        impl<'a> $T {
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
    )*};
}

tagged_methods!(
    dyn Tagged<'a>,
    dyn Tagged<'a> + Send,
    dyn Tagged<'a> + Sync,
    dyn Tagged<'a> + Send + Sync
);
