#![feature(core_intrinsics)]
#![feature(const_type_id)]
#![feature(const_type_name)]

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
pub mod dyno {
    use crate::any::TypeId;
    use core::marker::PhantomData;

    /// This trait is implemented by specific `Tag` types in order to allow
    /// describing a type which can be requested for a given lifetime `'a`.
    ///
    /// A few example implementations for type-driven `Tag`s can be found in the
    /// [`tag`] module, although crates may also implement their own tags for more
    /// complex types with internal lifetimes.
    pub trait Tag<'a>: Sized + 'static {
        /// The type of values which may be tagged by this `Tag` for the given
        /// lifetime.
        type Type: 'a;
    }

    /// Type-based `Tag` for `&'a T` types.
    pub struct Ref<T: ?Sized + 'static>(PhantomData<T>);

    impl<'a, T: ?Sized + 'static> Tag<'a> for Ref<T> {
        type Type = &'a T;
    }

    /// Type-based `Tag` for `&'a mut T` types.
    pub struct RefMut<T: ?Sized + 'static>(PhantomData<T>);

    impl<'a, T: ?Sized + 'static> Tag<'a> for RefMut<T> {
        type Type = &'a mut T;
    }

    /// Type-based `Tag` for static `T` types.
    pub struct Value<T: 'static>(PhantomData<T>);

    impl<'a, T: 'static> Tag<'a> for Value<T> {
        type Type = T;
    }

    /// Tag combinator to wrap the given tag's value in an `Option<T>`
    pub struct Optional<I>(PhantomData<I>);

    impl<'a, I: Tag<'a>> Tag<'a> for Optional<I> {
        type Type = Option<I::Type>;
    }

    mod private {
        pub trait Sealed {}
    }

    /// Sealed trait representing a type-erased tagged object.
    ///
    /// This trait is exclusively implemented by the `TagValue` type, and cannot be
    /// implemented outside of this crate due to being sealed.
    pub unsafe trait Tagged<'a>: private::Sealed + 'a {
        /// The `TypeId` of the `Tag` this value was tagged with.
        fn tag_id(&self) -> TypeId;
    }

    /// A concrete tagged value for a given tag `I`.
    ///
    /// This is the only type which implements the `Tagged` trait, and encodes
    /// additional information about the specific `Tag` into the type. This allows
    /// for multiple different tags to support overlapping value ranges, for
    /// example, both the `Ref<str>` and `Value<&'static str>` tags can be used to
    /// tag a value of type `&'static str`.
    #[repr(transparent)]
    pub struct TagValue<'a, I: Tag<'a>>(pub I::Type);

    impl<'a, I: Tag<'a>> private::Sealed for TagValue<'a, I> where I: Tag<'a> {}

    unsafe impl<'a, I> Tagged<'a> for TagValue<'a, I>
    where
        I: Tag<'a>,
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
                pub fn is<I>(&self) -> bool
                where
                    I: Tag<'a>,
                {
                    self.tag_id() == TypeId::of::<I>()
                }

                /// Returns some reference to the dynamic value if it is tagged with `I`,
                /// or `None` if it isn't.
                #[inline]
                pub fn downcast_ref<I>(&self) -> Option<&TagValue<'a, I>>
                where
                    I: Tag<'a>,
                {
                    if self.is::<I>() {
                        // SAFETY: Just checked whether we're pointing to a
                        // `TagValue<'a, I>`.
                        unsafe { Some(&*(self as *const Self as *const TagValue<'a, I>)) }
                    } else {
                        None
                    }
                }

                /// Returns some reference to the dynamic value if it is tagged with `I`,
                /// or `None` if it isn't.
                #[inline]
                pub fn downcast_mut<I>(&mut self) -> Option<&mut TagValue<'a, I>>
                where
                    I: Tag<'a>,
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
}

pub mod provider {
    use crate::dyno::{Optional, Ref, Tag, TagValue, Tagged, Value};

    /// Implementation detail shared between `Request<'a>` and `ConcreteRequest<'a, I>`.
    ///
    /// Generally this value is used through the `Request<'a>` type alias as a `&mut
    /// Request<'a>` out parameter, or constructed with the `ConcreteRequest<'a, I>`
    /// type alias.
    #[repr(transparent)]
    pub struct RequestImpl<T: ?Sized> {
        tagged: T,
    }

    /// An untyped request for a tagged value of a specific type.
    pub type Request<'a> = RequestImpl<dyn Tagged<'a> + 'a>;

    /// A concrete request for a tagged value. Can be coerced to `Request<'a>` to be
    /// passed to provider methods.
    pub type ConcreteRequest<'a, I> = RequestImpl<TagValue<'a, Optional<I>>>;

    impl<'a> Request<'a> {
        /// Check if the request is for a value with the given tag `I`. If it is,
        /// returns `true`.
        pub fn is<I>(&self) -> bool
        where
            I: Tag<'a>,
        {
            self.tagged.is::<Optional<I>>()
        }

        /// Attempts to provide a value with the given `Tag` to the request.
        pub fn provide<I>(&mut self, value: I::Type) -> &mut Self
        where
            I: Tag<'a>,
        {
            if let Some(res @ TagValue(None)) = self.tagged.downcast_mut::<Optional<I>>() {
                res.0 = Some(value);
            }
            self
        }

        /// Attempts to provide a value with the given `Tag` to the request.
        pub fn provide_with<I, F>(&mut self, f: F) -> &mut Self
        where
            I: Tag<'a>,
            F: FnOnce() -> I::Type,
        {
            if let Some(res @ TagValue(None)) = self.tagged.downcast_mut::<Optional<I>>() {
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
            if let Some(res @ TagValue(None)) = self.tagged.downcast_mut::<Optional<Value<T>>>() {
                res.0 = Some(f());
            }
            self
        }

        /// nrc extension.
        pub fn provide_ref<T: ?Sized + 'static>(&mut self, value: &'a T) -> &mut Self {
            if let Some(res @ TagValue(None)) = self.tagged.downcast_mut::<Optional<Ref<T>>>() {
                res.0 = Some(value);
            }
            self
        }
    }

    impl<'a, I> ConcreteRequest<'a, I>
    where
        I: Tag<'a>,
    {
        /// Construct a new unfulfilled concrete request for the given type. This
        /// can be coerced to a `Request<'a>` to pass to a type-erased provider
        /// method.
        pub fn new() -> Self {
            RequestImpl {
                tagged: TagValue(None),
            }
        }

        /// Take any provided value from this concrete request.
        pub fn take(self) -> Option<I::Type> {
            self.tagged.0
        }
    }

    /// Trait implemented by a type which can dynamically provide tagged values.
    pub trait Provider {
        fn provide<'a>(&'a self, request: &mut Request<'a>);
    }

    impl dyn Provider {
        /// Request a specific value by a given tag from the `Provider`.
        pub fn request<'a, I>(&'a self) -> Option<I::Type>
        where
            I: Tag<'a>,
        {
            let mut request = <ConcreteRequest<'a, I>>::new();
            self.provide(&mut request);
            request.take()
        }
    }
}

pub mod error {
    use crate::dyno::{Ref, Tag, Value};
    use crate::provider::{ConcreteRequest, Provider, Request};
    use core::fmt::Debug;

    pub trait MyError: Debug + Provider {
        fn provide_context<'a>(&'a self, request: &mut Request<'a>);
    }

    impl<T: MyError> Provider for T {
        fn provide<'a>(&'a self, request: &mut Request<'a>) {
            self.provide_context(request);
        }
    }

    impl dyn MyError {
        pub fn context_by_type_tag<'a, I>(&'a self) -> Option<I::Type>
        where
            I: Tag<'a>,
        {
            let mut request = <ConcreteRequest<'a, I>>::new();
            self.provide(&mut request);
            request.take()
        }

        pub fn context_value<T: 'static>(&self) -> Option<T> {
            let mut request = <ConcreteRequest<'_, Value<T>>>::new();
            self.provide(&mut request);
            request.take()
        }

        pub fn context_ref<'a, T: ?Sized + 'static>(&'a self) -> Option<&'a T> {
            let mut request = <ConcreteRequest<'a, Ref<T>>>::new();
            self.provide(&mut request);
            request.take()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::dyno;
    use crate::error::*;
    use crate::provider::Request;

    #[derive(Debug)]
    pub struct ConcreteError {
        name: String,
        array: Vec<String>,
    }

    impl MyError for ConcreteError {
        fn provide_context<'a>(&'a self, result: &mut Request<'a>) {
            result
                .provide_value(|| "Hello!".to_owned())
                .provide_ref(&*self.name);
        }
    }

    #[test]
    fn it_works() {
        let e: &dyn MyError = &ConcreteError {
            name: "Bob".to_owned(),
            array: vec![],
        };
        let s: String = e.context_value().unwrap();
        assert_eq!(&s, "Hello!");
        assert!(e.context_by_type_tag::<dyno::Value<i32>>().is_none());
        let s: &str = e.context_ref().unwrap();
        assert_eq!(s, "Bob");
        // let a: &[String] = e.context_ref().unwrap();
        // assert_eq!(a.len(), 0);
        // let s = e.context::<dyno::Ref<str>>().unwrap();
        // assert_eq!(s, "Boo!");
    }
}
