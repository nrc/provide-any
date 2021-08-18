#![feature(core_intrinsics)]
#![feature(const_type_id)]
#![feature(const_type_name)]
#![allow(incomplete_features)]
#![feature(trait_upcasting)]

/// https://doc.rust-lang.org/nightly/std/any/index.html
pub mod any {
    use core::fmt;
    use core::intrinsics;

    pub trait Any: 'static {
        fn type_id(&self) -> TypeId;
    }

    impl<T: 'static + ?Sized> Any for T {
        fn type_id(&self) -> TypeId {
            TypeId::of::<T>()
        }
    }

    impl fmt::Debug for dyn Any {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Any").finish_non_exhaustive()
        }
    }

    impl fmt::Debug for dyn Any + Send {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Any").finish_non_exhaustive()
        }
    }

    impl fmt::Debug for dyn Any + Send + Sync {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Any").finish_non_exhaustive()
        }
    }

    impl dyn Any {
        #[inline]
        pub fn is<T: Any>(&self) -> bool {
            // Get `TypeId` of the type this function is instantiated with.
            let t = TypeId::of::<T>();

            // Get `TypeId` of the type in the trait object (`self`).
            let concrete = self.type_id();

            // Compare both `TypeId`s on equality.
            t == concrete
        }

        #[inline]
        pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
            if self.is::<T>() {
                // SAFETY: just checked whether we are pointing to the correct type, and we can rely on
                // that check for memory safety because we have implemented Any for all types; no other
                // impls can exist as they would conflict with our impl.
                unsafe { Some(&*(self as *const dyn Any as *const T)) }
            } else {
                None
            }
        }

        #[inline]
        pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
            if self.is::<T>() {
                // SAFETY: just checked whether we are pointing to the correct type, and we can rely on
                // that check for memory safety because we have implemented Any for all types; no other
                // impls can exist as they would conflict with our impl.
                unsafe { Some(&mut *(self as *mut dyn Any as *mut T)) }
            } else {
                None
            }
        }
    }

    impl dyn Any + Send {
        #[inline]
        pub fn is<T: Any>(&self) -> bool {
            <dyn Any>::is::<T>(self)
        }

        #[inline]
        pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
            <dyn Any>::downcast_ref::<T>(self)
        }

        #[inline]
        pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
            <dyn Any>::downcast_mut::<T>(self)
        }
    }

    impl dyn Any + Send + Sync {
        #[inline]
        pub fn is<T: Any>(&self) -> bool {
            <dyn Any>::is::<T>(self)
        }

        #[inline]
        pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
            <dyn Any>::downcast_ref::<T>(self)
        }

        pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
            <dyn Any>::downcast_mut::<T>(self)
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
    pub struct TypeId {
        t: u64,
    }

    impl TypeId {
        pub const fn of<T: ?Sized + 'static>() -> TypeId {
            TypeId {
                t: intrinsics::type_id::<T>(),
            }
        }
    }

    pub const fn type_name<T: ?Sized>() -> &'static str {
        intrinsics::type_name::<T>()
    }

    pub const fn type_name_of_val<T: ?Sized>(_val: &T) -> &'static str {
        type_name::<T>()
    }
}

/// https://github.com/mystor/dyno/tree/min_magic
pub mod typed_provider {
    use crate::any::TypeId;

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
    }

    /// An untyped request for a tagged value of a specific type.
    pub struct Requisition<'a, 'b>(&'b mut RequisitionImpl<dyn Tagged<'a> + 'a>);
    // TODO is this useful? What about a Send or Sync version?
    // pub struct SendRequisition<'a, 'b>(&'b mut RequisitionImpl<dyn Tagged<'a> + 'a + Sync>);

    impl<'a, 'b> Requisition<'a, 'b> {
        /// Attempts to provide a value with the given `TypeTag` to the request.
        pub fn provide<I>(&mut self, value: I::Type) -> &mut Self
        where
            I: TypeTag<'a>,
        {
            if let Some(res @ TagValue(None)) = self.0.tagged.downcast_mut::<tags::Optional<I>>() {
                res.0 = Some(value);
            }
            self
        }

        /// Attempts to provide a value with the given `TypeTag` to the request.
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

        /// nrc extension.
        pub fn provide_value<T, F>(&mut self, f: F) -> &mut Self
        where
            T: 'static,
            F: FnOnce() -> T,
        {
            self.provide_with::<tags::Value<T>, F>(f)
        }

        /// nrc extension.
        pub fn provide_ref<T: ?Sized + 'static>(&mut self, value: &'a T) -> &mut Self {
            self.provide::<tags::Ref<T>>(value)
        }
    }

    /// A concrete request for a tagged value. Can be coerced to `Requisition<'a>` to be
    /// passed to provider methods.
    type ConcreteRequisition<'a, I> = RequisitionImpl<TagValue<'a, tags::Optional<I>>>;

    /// Implementation detail shared between `Requisition<'a>` and `ConcreteRequisition<'a, I>`.
    ///
    /// Generally this value is used through the `Requisition<'a>` type alias as a `&mut
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
        ($($T:ty),*) => {$(
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
        dyn Tagged<'a> //,
        // we only need these if we will pass Requisitions between threads,
        // which we don't for Error, but might for async.
                       // dyn Tagged<'a> + Send,
                       // dyn Tagged<'a> + Sync,
                       // dyn Tagged<'a> + Send + Sync
    );

    /// Trait implemented by a type which can dynamically provide tagged values.
    pub trait Provider {
        fn provide<'a>(&'a self, req: Requisition<'a, '_>);
    }

    /// Request a specific value by a given tag from the `Provider`.
    pub fn request_by_type_tag<'a, I>(provider: &'a dyn Provider) -> Option<I::Type>
    where
        I: TypeTag<'a>,
    {
        let mut req: ConcreteRequisition<'a, I> = RequisitionImpl {
            tagged: TagValue(None),
        };
        provider.provide(Requisition(&mut req));
        req.tagged.0
    }
}

pub mod error {
    use crate::typed_provider::{request_by_type_tag, tags, Provider, Requisition, TypeTag};
    use core::fmt::Debug;

    pub trait Error: Debug + Provider {
        fn provide_context<'a>(&'a self, _req: Requisition<'a, '_>) {}
    }

    impl<T: Error> Provider for T {
        fn provide<'a>(&'a self, req: Requisition<'a, '_>) {
            self.provide_context(req);
        }
    }

    impl dyn Error {
        pub fn get_context_by_type_tag<'a, I: TypeTag<'a>>(&'a self) -> Option<I::Type> {
            request_by_type_tag::<'_, I>(self)
        }

        pub fn get_context<T: 'static>(&self) -> Option<T> {
            request_by_type_tag::<'_, tags::Value<T>>(self)
        }

        pub fn get_context_ref<T: ?Sized + 'static>(&self) -> Option<&T> {
            request_by_type_tag::<'_, tags::Ref<T>>(self)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::*;
    use crate::typed_provider::{tags, Requisition};

    #[derive(Debug)]
    pub struct ConcreteError {
        name: String,
        array: Vec<String>,
    }

    impl Error for ConcreteError {
        fn provide_context<'a>(&'a self, mut req: Requisition<'a, '_>) {
            req.provide_value(|| "Hello!".to_owned())
                .provide_ref(&*self.name);
        }
    }

    #[test]
    fn it_works() {
        let e: &dyn Error = &ConcreteError {
            name: "Bob".to_owned(),
            array: vec![],
        };
        let s: String = e.get_context().unwrap();
        assert_eq!(&s, "Hello!");
        assert!(e.get_context_by_type_tag::<tags::Value<i32>>().is_none());
        let s: &str = e.get_context_ref().unwrap();
        assert_eq!(s, "Bob");
        // let a: &[String] = e.get_context_ref().unwrap();
        // assert_eq!(a.len(), 0);
        // let s = e.get_context_by_type_tag::<tags::Ref<str>>().unwrap();
        // assert_eq!(s, "Boo!");
    }
}
