use crate::error::*;
use crate::provide_any::{self, Demand, Provider};

// A concrete error type implemented by an application or library author.
#[derive(Debug)]
pub struct ConcreteError {
    name: String,
    array: Vec<String>,
}

impl Error for ConcreteError {
    fn provide_context<'a>(&'a self, req: &mut Demand<'a>) {
        // Provide a `String` value (a temporary value), a `&str` reference (references a field
        // of `self`), and a slice of `String`s.
        req.provide_value::<String, _>(|| "Hello!".to_owned())
            .provide_value::<Vec<String>, _>(|| self.array.clone())
            .provide_ref::<str>(&*self.name)
            .provide_ref::<[String]>(&*self.array);
    }
}

#[test]
fn access_context() {
    let e: &dyn Error = &mut ConcreteError {
        name: "Bob".to_owned(),
        array: vec!["Alice".to_owned()],
    };

    // Get context by value.
    let s: String = e.get_context().unwrap();
    assert_eq!(&s, "Hello!");

    // Get context by reference.
    let s: &[String] = e.get_context_ref().unwrap();
    assert_eq!(s, &*vec!["Alice".to_owned()]);

    let s: &str = e.get_context_ref().unwrap();
    assert_eq!(s, "Bob");

    let v: Vec<String> = e.get_context().unwrap();
    assert_eq!(v, vec!["Alice".to_owned()]);
}

// Implement Provider from a non-core crate.
// Also demonstrates use of mutable references.
trait Foo: Provider {}

impl dyn Foo {
    pub fn get_ref<T: ?Sized + 'static>(&self) -> Option<&T> {
        provide_any::request_ref(self)
    }

    pub fn get_mut<T: ?Sized + 'static>(&mut self) -> Option<&mut T> {
        provide_any::request_mut(self)
    }
}

struct Bar {
    s: String,
}

impl Foo for Bar {}

impl Provider for Bar {
    fn provide<'a>(&'a self, req: &mut Demand<'a>) {
        req.provide_ref::<String>(&self.s);
    }

    fn provide_mut<'a>(&'a mut self, req: &mut Demand<'a>) {
        // req.provide_mut::<String>(&mut self.s);
        req.with(self)
            .provide_mut::<_, String>(|this| &mut this.s)
            .provide_mut::<_, Bar>(|this| this);
    }
}

#[test]
fn foo() {
    let mut b = Bar {
        s: "Bob".to_owned(),
    };
    let f: &mut dyn Foo = &mut b;

    let s: &String = f.get_ref().unwrap();
    assert_eq!(s, "Bob");

    let s: &mut String = f.get_mut().unwrap();
    s.push_str(" and Alice");
    let _b: &mut Bar = f.get_mut().unwrap();
    assert_eq!(b.s, "Bob and Alice");
}
