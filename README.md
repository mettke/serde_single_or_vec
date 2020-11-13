# serde_single_or_vec

This crate provides the `SingleOrVec` Type which allows
parsing either a single type T or a vector of type T using serde.
This is required when a server either returns an array
if there are multiple values or one value if there is only one.

## Example

```rust

#[derive(Deserialize, Serialize)]
struct Response {
    single: SingleOrVec<'static, u8>,
    multiple: SingleOrVec<'static, u8>,
}

let json = r#"{
  "single": 0,
  "multiple": [
    0,
    1,
    2
  ]
}"#;
let res: Response = serde_json::from_str(json).unwrap();
assert_eq!(json, &serde_json::to_string_pretty(&res).unwrap());
```

## Format

By default the `SingleOrVec` Type deserializes its content either
to a single value or an array if it contains multiple values. To change
this behaviour, its possible to define the output format.

```rust

#[derive(Deserialize, Serialize)]
struct Response {
    single: SingleOrVec<'static, u8>,
    multiple: SingleOrVec<'static, u8>,
}

let json = "[0]";

let res: SingleOrVec<'_, u8, PreferSingle> = serde_json::from_str(json).unwrap();
assert_eq!("0", &serde_json::to_string(&res).unwrap());

let res: SingleOrVec<'_, u8, AlwaysVector> = serde_json::from_str(json).unwrap();
assert_eq!("[0]", &serde_json::to_string(&res).unwrap());
```

## Storage Backend

The default Backend is a `Vec<T>` and thus always results in an allocation.
An alternativ is to use a `Cow<'_, T>` as backend which only requires an allocation
for a single value.

Note that this is only valid when using or serializing this value, deserialisation
always allocates due to [serde#1852](https://github.com/serde-rs/serde/issues/1852)

```rust
let json = "[0,1,2]";
let res: SingleOrVec<'_, u8, PreferSingle, Cow<'_, [u8]>> = serde_json::from_str(json).unwrap();
assert_eq!(json, &serde_json::to_string(&res).unwrap());
```

## Custom Storage Type

Its also possible to implement Custom Storage Backends by using the `Storage` Trait.

```rust
use arrayvec::ArrayVec;

struct ArrayVecStorage {}

impl<T> Storage<'_, T> for ArrayVecStorage {
    type Backing = ArrayVec<[T; 4]>;

    fn single(ty: T) -> Self::Backing {
        let mut vec = ArrayVec::new();
        vec.push(ty);
        vec
    }

    fn get_first_with_len(b: &Self::Backing) -> Option<(&T, usize)> {
        b.split_first().map(|(t, r)| (t, r.len()))
    }
}

let json = "[0,1,2]";
let res: SingleOrVec<'_, u8, PreferSingle, ArrayVecStorage> = serde_json::from_str(json).unwrap();
assert_eq!(json, &serde_json::to_string(&res).unwrap());

let json = "0";
let res: SingleOrVec<'_, u8, PreferSingle, ArrayVecStorage> = serde_json::from_str(json).unwrap();
assert_eq!(json, &serde_json::to_string(&res).unwrap());
```

## `no_std`

It is possible to use this crate with `no_std`, however, like serde, either `std` or
`alloc` is required.

## License

Licensed under either of

* Apache License, Version 2.0
  ([LICENSE-APACHE](LICENSE-APACHE) or [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))
* MIT license
  ([LICENSE-MIT](LICENSE-MIT) or [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.


License: MIT OR Apache-2.0
