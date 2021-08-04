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
