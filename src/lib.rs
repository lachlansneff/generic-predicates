//! A taste of what const generics could enable in the future.
//! 
//! ```rust,no_run
//! #![feature(generic_const_exprs)]
//!
//! use generic_predicates::generic_predicates;
//!
//! generic_predicates! {
//!     pub fn foo<const N: usize>()
//!     where
//!         (N > 23, "`N` must be greater than 23")
//!     {
//!
//!     }
//! }
//!
//! fn main() {
//!     // This compiles.
//!     foo::<24>();
//!
//!     // This doesn't.
//!     foo::<23>();
//! }
//! ```

pub use generic_predicates_macro::generic_predicates;

pub mod __private {
    pub const fn assertion(predicate: bool, msg: &'static str) -> usize {
        assert!(predicate, "{}", msg);
        0
    }
}
