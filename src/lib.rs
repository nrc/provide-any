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

pub mod provider {
    use crate::any::TypeId;
    use core::marker::PhantomData;
    use core::mem;

    // Internal structs for identifying classes of types. These are never instantiated and are only
    // used for their type ids.
    // Note that this type can also identify `&'static T`, etc.
    struct ValueTypeIdTag<T: 'static>(PhantomData<T>);
    // The parameter type T is T in `&'a T`.
    struct RefTypeIdTag<T: ?Sized + 'static>(PhantomData<T>);
    // Could add RefMutTypeId, types for Cow, etc.

    // An initialization helper, id identifies the requested type, result points at memory where the
    // result is stored; this may be uninitialized.
    // `'a` is a lower bound on the lifetime of a reference stored into self.result.
    pub struct TypeIdentifiedInit<'a> {
        id: TypeId,
        // true only if self.result is initialized and a valid point to the type identified by self.id.
        init: bool,
        // SAFETY must not be read unless self.init == true.
        result: *mut u8,
        phantom: PhantomData<&'a u8>,
    }

    impl<'a> TypeIdentifiedInit<'a> {
        // If !self.init and T matches self.id, then execute `f` and store the result in self.result.
        // Supports builder pattern usage.
        pub fn set_if_uninit_with<T: 'static>(&mut self, f: impl Fn() -> T) -> &mut Self {
            if !self.init && TypeId::of::<ValueTypeIdTag<T>>() == self.id {
                unsafe {
                    // SAFETY: the type id check guarantees that the cast to `*mut T` is valid, we
                    // only write into the pointer, without reading or dropping contents.
                    (self.result as *mut T).write(f());
                }
                self.init = true;
            }

            self
        }
        pub fn set_if_uninit_ref<T: ?Sized + 'static>(&mut self, r: &'a T) -> &mut Self {
            if !self.init && TypeId::of::<RefTypeIdTag<T>>() == self.id {
                unsafe {
                    // SAFETY: in addition to the safety invariants for the value version above, we
                    // track `'a` in self to ensure that we only accept references here which can be
                    // assured to be valid for a given lifetime.
                    (self.result as *mut &'a T).write(r);
                }
                self.init = true;
            }

            self
        }
    }

    pub trait Provider {
        // The constrained lifetime here means that provided references can only be references to
        // fields on self (or that are guaranteed to live longer than self).
        fn provide<'a>(&'a self, _: &mut TypeIdentifiedInit<'a>);
    }

    pub fn get<T: 'static>(provider: &(impl Provider + ?Sized)) -> Option<T> {
        let mut result = mem::MaybeUninit::<T>::uninit();
        let mut tagged = TypeIdentifiedInit {
            id: TypeId::of::<ValueTypeIdTag<T>>(),
            init: false,
            result: result.as_mut_ptr() as *mut u8,
            phantom: PhantomData,
        };
        provider.provide(&mut tagged);
        if tagged.init {
            Some(unsafe { result.assume_init() })
        } else {
            None
        }
    }

    pub fn get_ref<'a, T: ?Sized + 'static>(
        provider: &'a (impl Provider + ?Sized),
    ) -> Option<&'a T> {
        let mut result = mem::MaybeUninit::<&'a T>::uninit();
        let mut tagged = TypeIdentifiedInit {
            id: TypeId::of::<RefTypeIdTag<T>>(),
            init: false,
            result: result.as_mut_ptr() as *mut u8,
            phantom: PhantomData,
        };
        provider.provide(&mut tagged);
        if tagged.init {
            Some(unsafe { result.assume_init() })
        } else {
            None
        }
    }
}

// Demonstrates how a client library uses the provider API by extending the Provider trait and delegating to
// provider functions.
pub mod error {
    use crate::provider::{self, Provider, TypeIdentifiedInit};
    use core::fmt::Debug;

    pub trait MyError: Provider + Debug {
        fn provide_context<'a>(&'a self, _: &mut TypeIdentifiedInit<'a>) {}
    }

    impl<T: MyError> Provider for T {
        fn provide<'a>(&'a self, init: &mut TypeIdentifiedInit<'a>) {
            self.provide_context(init);
        }
    }

    impl dyn MyError {
        pub fn context_ref<T: 'static + ?Sized>(&self) -> Option<&T> {
            provider::get_ref(self)
        }
        pub fn context<T: 'static>(&self) -> Option<T> {
            provider::get(self)
        }
    }
}

pub mod dyno {
    use core::any::TypeId;
    use core::marker::PhantomData;

    /// An identifier which may be used to tag a specific
    pub trait Tag<'a>: Sized + 'static {
        /// The type of values which may be tagged by this `Tag`.
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

    mod private {
        pub trait Sealed {}
    }

    /// Sealed trait representing a type-erased tagged object.
    pub trait Tagged<'a>: private::Sealed + 'a {
        /// The `TypeId` of the `Tag` this value was tagged with.
        fn tag_id(&self) -> TypeId;
    }

    /// Internal wrapper type with the same representation as a known external type.
    #[repr(transparent)]
    struct TaggedImpl<'a, I>
    where
        I: Tag<'a>,
    {
        _value: I::Type,
    }

    impl<'a, I> private::Sealed for TaggedImpl<'a, I> where I: Tag<'a> {}

    impl<'a, I> Tagged<'a> for TaggedImpl<'a, I>
    where
        I: Tag<'a>,
    {
        fn tag_id(&self) -> TypeId {
            TypeId::of::<I>()
        }
    }

    // FIXME: This should also handle the cases for `dyn Tagged<'a> + Send`,
    // `dyn Tagged<'a> + Send + Sync` and `dyn Tagged<'a> + Sync`...
    //
    // Should be easy enough to do it with a macro...
    impl<'a> dyn Tagged<'a> {
        /// Tag a reference to a concrete type with a given `Tag`.
        ///
        /// This is like an unsizing coercion, but must be performed explicitly to
        /// specify the specific tag.
        pub fn tag_ref<I>(value: &I::Type) -> &dyn Tagged<'a>
        where
            I: Tag<'a>,
        {
            // SAFETY: `TaggedImpl<'a, I>` has the same representation as `I::Type`
            // due to `#[repr(transparent)]`.
            unsafe { &*(value as *const I::Type as *const TaggedImpl<'a, I>) }
        }

        /// Tag a reference to a concrete type with a given `Tag`.
        ///
        /// This is like an unsizing coercion, but must be performed explicitly to
        /// specify the specific tag.
        pub fn tag_mut<I>(value: &mut I::Type) -> &mut dyn Tagged<'a>
        where
            I: Tag<'a>,
        {
            // SAFETY: `TaggedImpl<'a, I>` has the same representation as `I::Type`
            // due to `#[repr(transparent)]`.
            unsafe { &mut *(value as *mut I::Type as *mut TaggedImpl<'a, I>) }
        }

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
        pub fn downcast_ref<I>(&self) -> Option<&I::Type>
        where
            I: Tag<'a>,
        {
            if self.is::<I>() {
                // SAFETY: Just checked whether we're pointing to a
                // `TaggedImpl<'a, I>`, which was cast to from an `I::Type`.
                unsafe { Some(&*(self as *const dyn Tagged<'a> as *const I::Type)) }
            } else {
                None
            }
        }

        /// Returns some reference to the dynamic value if it is tagged with `I`,
        /// or `None` if it isn't.
        #[inline]
        pub fn downcast_mut<I>(&mut self) -> Option<&mut I::Type>
        where
            I: Tag<'a>,
        {
            if self.is::<I>() {
                // SAFETY: Just checked whether we're pointing to a
                // `TaggedImpl<'a, I>`, which was cast to from an `I::Type`.
                unsafe { Some(&mut *(self as *mut dyn Tagged<'a> as *mut I::Type)) }
            } else {
                None
            }
        }
    }
}

pub mod dyno2 {
    // https://github.com/mystor/dyno/tree/min_magic

    use core::any::TypeId;
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

pub mod dyno2_provider {
    use crate::dyno2::{Optional, Tag, TagValue, Tagged};

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

pub mod dyno2_error {
    use crate::dyno2::Tag;
    use crate::dyno2_provider::{ConcreteRequest, Provider, Request};
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
        pub fn context<'a, I>(&'a self) -> Option<I::Type>
        where
            I: Tag<'a>,
        {
            let mut request = <ConcreteRequest<'a, I>>::new();
            self.provide(&mut request);
            request.take()
        }
    }
}

pub mod dyno_provider {
    use crate::dyno::{Tag, Tagged};

    /// An untyped request for a value of a specific type.
    ///
    /// This type is generally used as an `&mut Request<'a>` outparameter.
    #[repr(transparent)]
    pub struct Request<'a> {
        tagged: dyn Tagged<'a> + 'a,
    }

    impl<'a> Request<'a> {
        /// Helper for performing transmutes as `Request<'a>` has the same layout as
        /// `dyn Tagged<'a> + 'a`, just with a different type!
        ///
        /// This is just to have our own methods on it, and less of the interface
        /// exposed on the `provide` implementation.
        fn wrap_tagged<'b>(t: &'b mut (dyn Tagged<'a> + 'a)) -> &'b mut Self {
            unsafe { &mut *(t as *mut (dyn Tagged<'a> + 'a) as *mut Request<'a>) }
        }

        pub fn is<I>(&self) -> bool
        where
            I: Tag<'a>,
        {
            self.tagged.is::<ReqTag<I>>()
        }

        pub fn provide<I>(&mut self, value: I::Type) -> &mut Self
        where
            I: Tag<'a>,
        {
            if let Some(res @ None) = self.tagged.downcast_mut::<ReqTag<I>>() {
                *res = Some(value);
            }
            self
        }

        pub fn provide_with<I, F>(&mut self, f: F) -> &mut Self
        where
            I: Tag<'a>,
            F: FnOnce() -> I::Type,
        {
            if let Some(res @ None) = self.tagged.downcast_mut::<ReqTag<I>>() {
                *res = Some(f());
            }
            self
        }
    }

    pub trait Provider {
        fn provide<'a>(&'a self, request: &mut Request<'a>);
    }

    impl dyn Provider {
        pub fn request<'a, I>(&'a self) -> Option<I::Type>
        where
            I: Tag<'a>,
        {
            request::<I, _>(|request| self.provide(request))
        }
    }

    pub fn request<'a, I, F>(f: F) -> Option<<I as Tag<'a>>::Type>
    where
        I: Tag<'a>,
        F: FnOnce(&mut Request<'a>),
    {
        let mut result: Option<<I as Tag<'a>>::Type> = None;
        f(Request::<'a>::wrap_tagged(
            <dyn Tagged>::tag_mut::<ReqTag<I>>(&mut result),
        ));
        result
    }

    /// Implementation detail: Specific `Tag` tag used by the `Request` code under
    /// the hood.
    ///
    /// Composition of `Tag` types!
    struct ReqTag<I>(I);
    impl<'a, I: Tag<'a>> Tag<'a> for ReqTag<I> {
        type Type = Option<I::Type>;
    }
}

pub mod dyno_error {
    use crate::dyno::Tag;
    use crate::dyno_provider::{request, Provider, Request};
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
        pub fn context<'a, I>(&'a self) -> Option<I::Type>
        where
            I: Tag<'a>,
        {
            request::<I, _>(|request| self.provide(request))
        }
    }
}

#[cfg(test)]
mod dyno_tests {
    use crate::dyno;
    use crate::dyno_error::*;
    use crate::dyno_provider::Request;

    #[derive(Debug)]
    pub struct ConcreteError {
        name: String,
        array: Vec<String>,
    }

    impl MyError for ConcreteError {
        fn provide_context<'a>(&'a self, result: &mut Request<'a>) {
            result
                //.provide_with(|| "Hello!".to_owned())
                .provide::<dyno::Ref<str>>(&*self.name);
        }
    }

    #[test]
    fn it_works() {
        let e: &dyn MyError = &ConcreteError {
            name: "Bob".to_owned(),
            array: vec![],
        };
        let s: &str = e.context::<dyno::Ref<str>>().unwrap();
        assert_eq!(s, "Bob");
    }
}

#[cfg(test)]
mod dyno2_tests {
    use crate::dyno2;
    use crate::dyno2_error::*;
    use crate::dyno2_provider::Request;

    #[derive(Debug)]
    pub struct ConcreteError {
        name: String,
        array: Vec<String>,
    }

    impl MyError for ConcreteError {
        fn provide_context<'a>(&'a self, result: &mut Request<'a>) {
            result
                //.provide_with::<dyno2::Value<String>, _>(|| "Hello!".to_owned())
                .provide::<dyno2::Ref<str>>(&*self.name);
        }
    }

    #[test]
    fn it_works() {
        let e: &dyn MyError = &ConcreteError {
            name: "Bob".to_owned(),
            array: vec![],
        };
        // let s: String = e.context::<dyno2::Value<String>>().unwrap();
        // assert_eq!(&s, "Hello!");
        assert!(e.context::<dyno2::Value<i32>>().is_none());
        let s: &str = e.context::<dyno2::Ref<str>>().unwrap();
        assert_eq!(s, "Bob");
        // let a: &[String] = e.context_ref().unwrap();
        // assert_eq!(a.len(), 0);
        // let s = e.context::<dyno2::Ref<str>>().unwrap();
        // assert_eq!(s, "Boo!");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    pub struct ConcreteError {
        name: String,
        array: Vec<String>,
    }

    impl error::MyError for ConcreteError {
        fn provide_context<'a>(&'a self, result: &mut provider::TypeIdentifiedInit<'a>) {
            // Can't reference s because it doesn't live long enough.
            // let s = "foo".to_owned();
            result
                .set_if_uninit_with(|| "Hello!".to_owned())
                .set_if_uninit_ref(&*self.name)
                // .set_if_uninit_ref(&s)
                .set_if_uninit_ref::<[String]>(&self.array)
                .set_if_uninit_with(|| "Boo!");
        }
    }

    #[test]
    fn it_works() {
        let e: &dyn error::MyError = &ConcreteError {
            name: "Bob".to_owned(),
            array: vec![],
        };
        let s: String = e.context().unwrap();
        assert_eq!(&s, "Hello!");
        assert!(e.context::<i32>().is_none());
        let s: &str = e.context_ref::<str>().unwrap();
        assert_eq!(s, "Bob");
        let a: &[String] = e.context_ref().unwrap();
        assert_eq!(a.len(), 0);
        let s = e.context::<&str>().unwrap();
        assert_eq!(s, "Boo!");
    }
}
