use crate::provide_any::{self, Demand, Provider};
use core::fmt::Debug;

// Replaces `std::error::Error`, note the additional `Provider` trait bound.
pub trait Error: Debug + Provider {
    // Optional method for implementers to provide additional context.
    fn provide_context<'a>(&'a self, _req: &mut Demand<'a>) {}
}

// Blanket impl of `Provider` so that the bound is backwards compatible and implementers do not
// need to be aware of the `provide_any` API.
impl<T: Error> Provider for T {
    fn provide<'a>(&'a self, req: &mut Demand<'a>) {
        // Delegate to `Error::provide_context`
        self.provide_context(req);
    }
}

// Methods on `Error` trait objects.
impl dyn Error {
    /// Common case: get a reference to a field of the error.
    pub fn get_context_ref<T: ?Sized + 'static>(&self) -> Option<&T> {
        provide_any::request_ref(self)
    }

    /// Get a temporary value.
    pub fn get_context<T: 'static>(&self) -> Option<T> {
        provide_any::request_value(self)
    }
}
