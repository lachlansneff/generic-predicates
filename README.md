# A taste of what const generics could enable in the future

```rust
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
    // This compiles.
    foo::<24>();

    // This doesn't.
    foo::<23>();
}
```
