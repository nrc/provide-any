use crate::error::*;
use crate::provide_any::{tags, Requisition};

// A concrete error type implemented by an application or library author.
#[derive(Debug)]
pub struct ConcreteError {
    name: String,
    array: Vec<String>,
}

impl Error for ConcreteError {
    fn provide_context<'a>(&'a self, mut req: Requisition<'a, '_>) {
        // Provide a `String` value (a temporary value), a `&str` reference (references a field
        // of `self`), and a slice of `String`s.
        req.provide_value::<String, _>(|| "Hello!".to_owned())
            .provide_ref::<str>(&*self.name)
            .provide_ref::<[String]>(&*self.array);
    }
}

#[test]
fn access_context() {
    let e: &dyn Error = &ConcreteError {
        name: "Bob".to_owned(),
        array: vec!["Alice".to_owned()],
    };

    // Get context by value.
    let s: String = e.get_context().unwrap();
    assert_eq!(&s, "Hello!");

    // Get context by reference.
    let s: &str = e.get_context_ref().unwrap();
    assert_eq!(s, "Bob");

    // Get context by reference.
    let s: &[String] = e.get_context_ref().unwrap();
    assert_eq!(s, &*vec!["Alice".to_owned()]);

    // Use the fully-general API to get an `i32`, this operation fails since `ConcreteError` does
    // not provide `i32` context.
    assert!(e.get_context_by_type_tag::<tags::Value<i32>>().is_none());
}
