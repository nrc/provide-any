#![allow(incomplete_features)]
// We use trait upcasting in the implementation, it just makes things easier though, we can live
// without it.
#![feature(trait_upcasting)]

// This crate is an implementation for an object provider API in core, proposed in [RFC XXXX](TODO).

// The proposed API, a new module in core.
pub mod provide_any;
// Demonstrates how the proposed API could be used by the `Error` trait. These changes would be made
// to `std::error`.
pub mod error;

// Examples of user code using the proposed API via the `Error` trait.
#[cfg(test)]
mod tests;
