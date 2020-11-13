//! This crate provides the `SingleOrVec` Type which allows
//! parsing either a single type T or a vector of type T using serde.
//! This is required when a server either returns an array
//! if there are multiple values or one value if there is only one.
//!
//! # Example
//!
//! ```rust
//! # use serde_single_or_vec::SingleOrVec;
//! # use serde::{Deserialize, Serialize};
//!
//! #[derive(Deserialize, Serialize)]
//! struct Response {
//!     single: SingleOrVec<'static, u8>,
//!     multiple: SingleOrVec<'static, u8>,
//! }
//!
//! let json = r#"{
//!   "single": 0,
//!   "multiple": [
//!     0,
//!     1,
//!     2
//!   ]
//! }"#;
//! let res: Response = serde_json::from_str(json).unwrap();
//! assert_eq!(json, &serde_json::to_string_pretty(&res).unwrap());
//! ```
//!
//! # Format
//!
//! By default the `SingleOrVec` Type deserializes its content either
//! to a single value or an array if it contains multiple values. To change
//! this behaviour, its possible to define the output format.
//!
//! ```rust
//! # use serde_single_or_vec::{SingleOrVec, PreferSingle, AlwaysVector};
//! # use serde::{Deserialize, Serialize};
//!
//! #[derive(Deserialize, Serialize)]
//! struct Response {
//!     single: SingleOrVec<'static, u8>,
//!     multiple: SingleOrVec<'static, u8>,
//! }
//!
//! let json = "[0]";
//!
//! let res: SingleOrVec<'_, u8, PreferSingle> = serde_json::from_str(json).unwrap();
//! assert_eq!("0", &serde_json::to_string(&res).unwrap());
//!
//! let res: SingleOrVec<'_, u8, AlwaysVector> = serde_json::from_str(json).unwrap();
//! assert_eq!("[0]", &serde_json::to_string(&res).unwrap());
//! ```
//!
//! # Storage Backend
//!
//! The default Backend is a `Vec<T>` and thus always results in an allocation. 
//! An alternativ is to use a `Cow<'_, T>` as backend which only requires an allocation 
//! for a single value.
//!
//! Note that this is only valid when using or serializing this value, deserialisation
//! always allocates due to [serde#1852](https://github.com/serde-rs/serde/issues/1852)
//!
//! ```rust
//! # use std::borrow::Cow;
//! # use serde_single_or_vec::{SingleOrVec, PreferSingle};
//! let json = "[0,1,2]";
//! let res: SingleOrVec<'_, u8, PreferSingle, Cow<'_, [u8]>> = serde_json::from_str(json).unwrap();
//! assert_eq!(json, &serde_json::to_string(&res).unwrap());
//! ```
//!
//! # Custom Storage Type
//!
//! Its also possible to implement Custom Storage Backends by using the `Storage` Trait.
//!
//! ```rust
//! # use serde_single_or_vec::{SingleOrVec, PreferSingle, Storage};
//! use arrayvec::ArrayVec;
//!
//! struct ArrayVecStorage {}
//!
//! impl<T> Storage<'_, T> for ArrayVecStorage {
//!     type Backing = ArrayVec<[T; 4]>;
//!     
//!     fn single(ty: T) -> Self::Backing {
//!         let mut vec = ArrayVec::new();
//!         vec.push(ty);
//!         vec
//!     }
//!
//!     fn get_first_with_len(b: &Self::Backing) -> Option<(&T, usize)> {
//!         b.split_first().map(|(t, r)| (t, r.len()))
//!     }
//! }
//! 
//! let json = "[0,1,2]";
//! let res: SingleOrVec<'_, u8, PreferSingle, ArrayVecStorage> = serde_json::from_str(json).unwrap();
//! assert_eq!(json, &serde_json::to_string(&res).unwrap());
//! 
//! let json = "0";
//! let res: SingleOrVec<'_, u8, PreferSingle, ArrayVecStorage> = serde_json::from_str(json).unwrap();
//! assert_eq!(json, &serde_json::to_string(&res).unwrap());
//! ```
//! 
//! # `no_std`
//! 
//! It is possible to use this crate with `no_std`, however, like serde, either `std` or 
//! `alloc` is required.
//!
//! # License
//!
//! Licensed under either of
//!
//! * Apache License, Version 2.0
//!   ([LICENSE-APACHE](LICENSE-APACHE) or [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))
//! * MIT license
//!   ([LICENSE-MIT](LICENSE-MIT) or [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))
//!
//! at your option.
//!
//! # Contribution
//!
//! Unless you explicitly state otherwise, any contribution intentionally submitted
//! for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
//! dual licensed as above, without any additional terms or conditions.
//!

#![warn(
    absolute_paths_not_starting_with_crate,
    anonymous_parameters,
    box_pointers,
    confusable_idents,
    deprecated_in_future,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    indirect_structural_match,
    keyword_idents,
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_copy_implementations,
    missing_crate_level_docs,
    missing_debug_implementations,
    missing_docs,
    missing_doc_code_examples,
    non_ascii_idents,
    private_doc_tests,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unaligned_references,
    unreachable_pub,
    unsafe_code,
    unstable_features,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    variant_size_differences
)]
#![warn(
    clippy::cargo,
    clippy::complexity,
    clippy::correctness,
    clippy::nursery,
    clippy::pedantic,
    clippy::perf,
    clippy::style
)]
#![allow(clippy::implicit_return, clippy::shadow_unrelated)]

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(all(not(feature = "std"), not(feature = "alloc")))]
core::compile_error!("This crate requires either the std or the alloc feature");

use serde::{
    de::{Deserializer, Error},
    private::de::{Content, ContentRefDeserializer},
    ser::Serializer,
    Deserialize, Serialize,
};
use core::{marker::PhantomData, cmp::Ordering, hash::{Hash, Hasher}, ops::{Deref, DerefMut}, fmt};
#[cfg(feature = "std")]
use std::{borrow::Cow, vec, vec::Vec};
#[cfg(feature = "alloc")]
use alloc::{borrow::Cow, vec, vec::Vec};

/// Specifies the internal Storage Type of the Sequence
#[allow(unused_lifetimes)]
pub trait Storage<'a, T> {
    /// Type to use as storage type;
    type Backing;

    /// Creates the Storage for a single value
    fn single(ty: T) -> Self::Backing;

    /// Returns the first element of the vector if available
    /// and the size of the remaining vector without the first element
    fn get_first_with_len(b: &Self::Backing) -> Option<(&T, usize)>;
}

impl<T> Storage<'_, T> for Vec<T> {
    type Backing = Self;

    fn single(ty: T) -> Self::Backing {
        vec![ty]
    }

    fn get_first_with_len(b: &Self::Backing) -> Option<(&T, usize)> {
        b.split_first().map(|(t, r)| (t, r.len()))
    }
}

impl<'a, T: 'a + Clone> Storage<'a, T> for Cow<'a, [T]> {
    type Backing = Self;

    fn single(ty: T) -> Self::Backing {
        Cow::Owned(vec![ty])
    }

    fn get_first_with_len(b: &Self::Backing) -> Option<(&T, usize)> {
        b.split_first().map(|(t, r)| (t, r.len()))
    }
}

/// Specifies whether Serializsation should fall back to a single value or not
pub trait Format {
    /// If this returns true, the serializer will returns a single value if there
    /// is only one in the vector. Otherwise it will always returns a vector
    fn use_single() -> bool;
}

#[derive(Debug, Clone, Copy)]
/// Returns a single value on serialisation if there is only one in the vector
pub struct PreferSingle {}

impl Format for PreferSingle {
    fn use_single() -> bool {
        true
    }
}

#[derive(Debug, Clone, Copy)]
/// Always returns a vector on serialisation
pub struct AlwaysVector {}

impl Format for AlwaysVector {
    fn use_single() -> bool {
        false
    }
}

/// Provides a type which can be used to parse server respsonses
/// which either return an array or only a single value.
#[allow(single_use_lifetimes)]
pub struct SingleOrVec<'a, T, F: Format = PreferSingle, S: Storage<'a, T> = Vec<T>> {
    data: S::Backing,
    format: PhantomData<F>,
}

impl<'a, T, F, S, B> Default for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T, Backing = B>,
    B: Default,
{
    fn default() -> Self {
        Self {
            data: B::default(),
            format: PhantomData,
        }
    }
}

impl<'a, T, F, S, B> Clone for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T, Backing = B>,
    B: Clone,
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            format: PhantomData,
        }
    }
}

impl<'a, T, F, S, B> fmt::Debug for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T, Backing = B>,
    B: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SingleOrVec")
            .field("data", &self.data)
            .finish()
    }
}

impl<'a, T, F, S, B> Eq for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T, Backing = B>,
    B: Eq,
{
}

impl<'a, T, F, F2, S, B> PartialEq<SingleOrVec<'a, T, F2, S>> for SingleOrVec<'a, T, F, S>
where
    F: Format,
    F2: Format,
    S: Storage<'a, T, Backing = B>,
    B: PartialEq,
{
    fn eq(&self, other: &SingleOrVec<'a, T, F2, S>) -> bool {
        self.data.eq(&other.data)
    }
}

impl<'a, T, F, S, B> Ord for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T, Backing = B>,
    B: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.data.cmp(&other.data)
    }
}

impl<'a, T, F, F2, S, B> PartialOrd<SingleOrVec<'a, T, F2, S>> for SingleOrVec<'a, T, F, S>
where
    F: Format,
    F2: Format,
    S: Storage<'a, T, Backing = B>,
    B: PartialOrd,
{
    fn partial_cmp(&self, other: &SingleOrVec<'a, T, F2, S>) -> Option<Ordering> {
        self.data.partial_cmp(&other.data)
    }
}

impl<'a, T, F, S, B> Hash for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T, Backing = B>,
    B: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data.hash(state)
    }
}

impl<'a, T, F, S> SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T>,
{
    #[must_use]
    /// Creates the type using a given array
    pub fn new(ty: S::Backing) -> Self {
        Self {
            data: ty,
            format: PhantomData,
        }
    }

    #[must_use]
    /// Creates the type using a single value
    pub fn new_single(ty: T) -> Self {
        Self {
            data: S::single(ty),
            format: PhantomData,
        }
    }
}

impl<'a, T, F, S> Deref for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T>,
{
    type Target = S::Backing;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'a, T, F, S> DerefMut for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T>,
{
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<'a, T, F, S, B> From<T> for SingleOrVec<'a, T, F, S>
where
    F: Format,
    S: Storage<'a, T, Backing = B>,
    B: From<T>,
{
    fn from(d: T) -> Self {
        Self {
            data: B::from(d),
            format: PhantomData,
        }
    }
}

impl<'a, T, F, St, B> Serialize for SingleOrVec<'a, T, F, St>
where
    T: Serialize,
    F: Format,
    St: Storage<'a, T, Backing = B>,
    B: Serialize,
{
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        if F::use_single() {
            if let Some((first, len)) = St::get_first_with_len(&self.data) {
                if len == 0 {
                    return first.serialize(serializer);
                }
            }
        }
        self.data.serialize(serializer)
    }
}

impl<'de, 'a, T, F, S, B> Deserialize<'de> for SingleOrVec<'a, T, F, S>
where
    T: Deserialize<'de>,
    F: Format,
    S: Storage<'a, T, Backing = B>,
    B: Deserialize<'de>,
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let content = Content::deserialize(deserializer)?;
        <T as Deserialize>::deserialize(ContentRefDeserializer::<D::Error>::new(&content))
            .map(S::single)
            .or_else(|_| B::deserialize(ContentRefDeserializer::<D::Error>::new(&content)))
            .map(Self::new)
            .map_err(|_| Error::custom("Expected either a sequence or a single value"))
    }
}

#[cfg(test)]
mod tests {
    use super::SingleOrVec;
    use arrayvec as _;
    use serde::{Deserialize, Serialize};
    #[cfg(feature = "std")]
    use std::{vec, string::String, vec::Vec};
    #[cfg(feature = "alloc")]
    use alloc::{vec, string::String, vec::Vec};

    macro_rules! multiple {
        ($t:ty, $i:expr, $e:expr) => {
            let input: SingleOrVec<'_, $t> = SingleOrVec::new(vec![$i, $i, $i]);
            let json_expected = $e;

            let json = serde_json::to_string(&input).expect("Unable to deserialize");
            assert_eq!(
                json_expected, json,
                "Deserializing not matching with expected value"
            );

            let output: SingleOrVec<'_, $t> = serde_json::from_str(&json).expect("Unable to parse");
            assert_eq!(output, input, "Output and input not matching up")
        };
    }

    macro_rules! single {
        ($t:ty, $i:expr, $e:expr) => {
            let input: SingleOrVec<'_, $t> = SingleOrVec::new_single($i);
            let json_expected = $e;

            let json = serde_json::to_string(&input).expect("Unable to deserialize");
            assert_eq!(
                json_expected, json,
                "Deserializing not matching with expected value"
            );

            let output: SingleOrVec<'_, $t> = serde_json::from_str(&json).expect("Unable to parse");
            assert_eq!(output, input, "Output and input not matching up")
        };
    }

    macro_rules! numbers {
        ($x:ident) => {
            #[test]
            fn $x() {
                multiple!($x, 0, "[0,0,0]");
                single!($x, 0, "0");
                single!($x, 1, "1");
            }
        };
        ($x:ident, $($y:ident),+) => (
            numbers!($x);
            numbers!($($y),+);
        );
    }

    macro_rules! decimals {
        ($x:ident) => {
            #[test]
            fn $x() {
                multiple!($x, 0.0, "[0.0,0.0,0.0]");
                single!($x, 0.0, "0.0");
                single!($x, 1.1, "1.1");
            }
        };
        ($x:ident, $($y:ident),+) => (
            decimals!($x);
            decimals!($($y),+);
        );
    }

    #[test]
    fn bool() {
        multiple!(bool, true, "[true,true,true]");
        single!(bool, false, "false");
        single!(bool, true, "true");
    }

    numbers!(i8, i16, i32, i64, u8, u16, u32, u64); // i128 and u128 do not work
    decimals!(f32, f64);

    #[test]
    fn char() {
        multiple!(char, ' ', "[\" \",\" \",\" \"]");
        single!(char, ' ', "\" \"");
    }

    #[test]
    fn str() {
        multiple!(&str, " ", "[\" \",\" \",\" \"]");
        single!(&str, " ", "\" \"");
    }

    #[test]
    fn string() {
        multiple!(String, String::from(" "), "[\" \",\" \",\" \"]");
        single!(String, String::from(" "), "\" \"");
    }

    // #[test]
    // fn bytes() {
    //     let bytes: &[u8] = &[0, 1, 2];
    //     multiple!(&[u8], bytes, "[[0,1,2],[0,1,2],[0,1,2]]");
    //     single!(&[u8], bytes, "[0,1,2]");
    // }

    #[test]
    fn bytes_buf() {
        multiple!(Vec<u8>, vec![0, 1, 2], "[[0,1,2],[0,1,2],[0,1,2]]");
        single!(Vec<u8>, vec![0, 1, 2], "[0,1,2]");
    }

    #[test]
    fn none() {
        multiple!(Option<()>, None, "[null,null,null]");
        single!(Option<()>, None, "null");
    }

    #[test]
    fn some() {
        multiple!(Option<u8>, Some(0_u8), "[0,0,0]");
        single!(Option<u8>, Some(0_u8), "0");
    }

    #[test]
    fn unit() {
        multiple!((), (), "[null,null,null]");
        single!((), (), "null");
    }

    #[test]
    fn strct() {
        #[derive(Debug, Deserialize, PartialEq, Serialize)]
        struct TestStruct {
            test: u8,
        }

        multiple!(
            TestStruct,
            TestStruct { test: 0 },
            "[{\"test\":0},{\"test\":0},{\"test\":0}]"
        );
        single!(TestStruct, TestStruct { test: 0 }, "{\"test\":0}");
    }

    #[test]
    fn seq() {
        multiple!(
            Vec<&str>,
            vec!["a", "b", "c"],
            "[[\"a\",\"b\",\"c\"],[\"a\",\"b\",\"c\"],[\"a\",\"b\",\"c\"]]"
        );
        single!(Vec<&str>, vec!["a", "b", "c"], "[\"a\",\"b\",\"c\"]");
    }

    #[test]
    fn enm() {
        #[derive(Debug, Deserialize, PartialEq, Serialize)]
        enum TestEnum {
            One,
            Two,
        }

        multiple!(TestEnum, TestEnum::Two, "[\"Two\",\"Two\",\"Two\"]");
        single!(TestEnum, TestEnum::One, "\"One\"");
    }

    #[test]
    fn rec() {
        multiple!(
            SingleOrVec<'_, &str>,
            SingleOrVec::new(vec!["a", "b", "c"]),
            "[[\"a\",\"b\",\"c\"],[\"a\",\"b\",\"c\"],[\"a\",\"b\",\"c\"]]"
        );
        single!(
            SingleOrVec<'_, &str>,
            SingleOrVec::new(vec!["a", "b", "c"]),
            "[\"a\",\"b\",\"c\"]"
        );
        single!(SingleOrVec<'_, &str>, SingleOrVec::new(vec!["a"]), "\"a\"");
    }

    #[test]
    fn empty() {
        single!(
            SingleOrVec<'_, &str>,
            SingleOrVec::new(vec![]),
            "[]"
        );
    }
}
