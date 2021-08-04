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

pub mod tagged {
    use crate::any::TypeId;
    use core::marker::PhantomData;

    pub trait Tagged<'a> {
        type Tag: ?Sized + 'static;

        fn type_id() -> TypeId {
            TypeId::of::<Self::Tag>()
        }
    }

    pub struct ValueTypeId<T: ?Sized + 'static>(PhantomData<T>);
    pub struct RefTypeId<T: ?Sized + 'static>(PhantomData<T>);
    pub struct RefMutTypeId<T: ?Sized + 'static>(PhantomData<T>);

    // User adds Tagged impls for any values they like, e.g.,
    impl Tagged<'static> for String {
        type Tag = ValueTypeId<String>;
    }
    impl Tagged<'static> for i32 {
        type Tag = ValueTypeId<i32>;
    }
    impl<'a, T: ?Sized + 'static> Tagged<'a> for &'a T {
        type Tag = RefTypeId<T>;
    }
    impl<'a, T: ?Sized + 'static> Tagged<'a> for &'a mut T {
        type Tag = RefMutTypeId<T>;
    }
}

pub mod provider {
    use crate::any::TypeId;
    use crate::tagged::Tagged;
    use core::mem;

    pub struct TypeIdentifiedInit {
        id: TypeId,
        init: bool,
        result: *mut u8,
    }

    // TODO lifetimes?
    impl TypeIdentifiedInit {
        pub fn set_if_uninit_with<T: Tagged<'static>>(&mut self, f: impl Fn() -> T) -> &mut Self {
            if !self.init && T::type_id() == self.id {
                unsafe {
                    (self.result as *mut T).write(f());
                }
                self.init = true;
            }

            self
        }
        pub fn set_if_uninit_ref<'a, T: ?Sized>(&mut self, r: &'a T) -> &mut Self
            where &'a T: Tagged<'a>
        {
            if !self.init && <&'a T>::type_id() == self.id {
                unsafe {
                    (self.result as *mut &'a T).write(r);
                }
                self.init = true;
            }

            self
        }
    }

    pub trait Provider {
        fn provide(&self, _: &mut TypeIdentifiedInit);
    }

    pub fn get<T: Tagged<'static>>(provider: &(impl Provider + ?Sized)) -> Option<T> {
        let mut result = mem::MaybeUninit::<T>::uninit();
        let mut tagged = TypeIdentifiedInit {
            id: T::type_id(),
            init: false,
            result: result.as_mut_ptr() as *mut u8,
        };
        provider.provide(&mut tagged);
        if tagged.init {
            Some(unsafe { result.assume_init() })
        } else {
            None
        }
    }
}

pub mod error {
    use crate::provider::{self, Provider, TypeIdentifiedInit};
    use crate::tagged::Tagged;
    use core::fmt::Debug;

    pub trait MyError: Provider + Debug {
        fn provide_context(&self, _: &mut TypeIdentifiedInit) {}
    }

    impl<T: MyError> Provider for T {
        fn provide(&self, init: &mut TypeIdentifiedInit) {
            self.provide_context(init);
        }
    }

    impl dyn MyError {
        pub fn context<T: Tagged<'static>>(&self) -> Option<T> {
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
    }

    impl error::MyError for ConcreteError {
        fn provide_context(&self, result: &mut provider::TypeIdentifiedInit) {
            result
                .set_if_uninit_with(|| "Hello!".to_owned())
                .set_if_uninit_ref(&*self.name);
        }
    }

    #[test]
    fn it_works() {
        let e: &dyn error::MyError = &ConcreteError {
            name: "Bob".to_owned(),
        };
        let s: String = e.context().unwrap();
        assert_eq!(&s, "Hello!");
        // Error, this must be getting &'static str - unsafe lifetime
        let s: &str = e.context().unwrap();
        assert_eq!(s, "Bob");
        assert!(e.context::<i32>().is_none());
    }
}
