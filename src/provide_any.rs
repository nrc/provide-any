use core::any::TypeId;

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

impl<'a, 'b> Requisition<'a, 'b> {
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
    dyn Tagged<'a>,
    dyn Tagged<'a> + Send,
    dyn Tagged<'a> + Sync,
    dyn Tagged<'a> + Send + Sync
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
