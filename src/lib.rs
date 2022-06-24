pub use generic_predicates_macro::generic_predicates;

pub mod __private {
    pub const fn assertion(predicate: bool, msg: &'static str) -> usize {
        assert!(predicate, "{}", msg);
        0
    }
}
