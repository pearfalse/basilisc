pub mod support;
pub mod token_data;
pub mod line_numbers;
pub mod latin1;
pub mod unpack;
pub mod pack;

// include meta-src files that we want
#[path = "../meta-src/keyword.rs"] pub mod keyword;
#[path = "../meta-src/token_iter.rs"] pub mod token_iter;
#[path = "../meta-src/subarray.rs"] mod subarray;
#[cfg(any(test, doc))]
#[path = "../meta-src/cooked_keyword.rs"] mod cooked_keyword;