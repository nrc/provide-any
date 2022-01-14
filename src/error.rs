use crate::provide_any::{request_by_type_tag, tags, Provider, Requisition, TypeTag};
use core::fmt::Debug;

// Replaces `std::error::Error`, note the additional `Provider` trait bound.
pub trait Error: Debug + Provider {
    // Optional method for implementers to provide additional context.
    fn provide_context<'a>(&'a self, _req: &mut Requisition<'a>) {}
}

// Blanket impl of `Provider` so that the bound is backwards compatible and implementers do not
// need to be aware of the `provide_any` API.
impl<T: Error> Provider for T {
    fn provide<'a>(&'a self, req: &mut Requisition<'a>) {
        // Delegate to `Error::provide_context`
        self.provide_context(req);
    }
}

// Methods on `Error` trait objects.
impl dyn Error {
    /// Common case: get a reference to a field of the error.
    pub fn get_context_ref<T: ?Sized + 'static>(&self) -> Option<&T> {
        request_by_type_tag::<'_, tags::Ref<T>>(self)
    }

    /// Get a temporary value.
    pub fn get_context<T: 'static>(&self) -> Option<T> {
        request_by_type_tag::<'_, tags::Value<T>>(self)
    }

    /// Fully general, but uncommon case. Get context using a type tag, allows for fetching context
    /// with complex lifetimes.
    pub fn get_context_by_type_tag<'a, I: TypeTag<'a>>(&'a self) -> Option<I::Type> {
        request_by_type_tag::<'_, I>(self)
    }
}
