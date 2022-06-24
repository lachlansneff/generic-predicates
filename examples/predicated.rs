#![feature(generic_const_exprs)]

use generic_predicates::generic_predicates;

generic_predicates! {
    pub fn foo<const N: usize>()
    where
        (N > 23, "`N` must be greater than 23")
    {

    }
}

fn main() {
    foo::<24>();
    foo::<23>();
}
