//! Container module for [`SubArray`] and related types.

use std::{num::NonZeroUsize, ops::Index};

/// A slice wrapper that abstracts a slice where the first few elements are logically missing.
///
/// `SubArray` exists to reduce the memory usage of arrays where a large number of values from
/// index 0 have no logical meaning. For instance, in the direct token map, the first 127 values
/// will never map to anything, so we shouldn't need to waste 1500+ bytes just to simplify the
/// index math at call sites.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SubArray<'a, T> {
	slice: &'a [T],
	from: usize,
}

impl<'a, T> SubArray<'a, T> {
	/// Constructs a new `SubArray` from a slice, and logical index that `slice[0]` maps to.
	pub const fn new(slice: &'a [T], from: usize) -> Self {
		// ensure top of slice is in range
		if from.checked_add(slice.len().saturating_sub(1)).is_none() {
			panic!("slice is too big for given start offset");
		}

		SubArray { slice, from }
	}

	/// Gets the value at this logical index.
	pub fn get(&self, index: usize) -> Option<&'a T> {
		index.checked_sub(self.from).and_then(|raw_idx| self.slice.get(raw_idx))
	}

	/// Gets the raw slice within the subarray.
	pub fn as_raw_slice(&self) -> &'a [T] {
		self.slice
	}
}

impl<'a, T> SubArray<'a, Option<T>> {
	/// Constructs a [`FullIter`] for this SubArray.
	#[cfg_attr(not(test), allow(dead_code))]
	pub fn full_iter(&self) -> FullIter<'a, T> {
		FullIter::new(self)
	}
}

impl<'a, T> Index<usize> for SubArray<'a, T> {
	type Output = T;

	fn index(&self, index: usize) -> &Self::Output {
		self.get(index).expect("index out of range")
	}
}

/// Iterates over the values in the slice, after returning multiple `None` values that
/// are logically at the start.
pub struct FullIter<'a, T> {
	slice: core::slice::Iter<'a, Option<T>>,
	skip_count: Option<NonZeroUsize>,
}

impl<'a, T> FullIter<'a, T> {
	/// Constructs a new iterator.
	fn new(src: &SubArray<'a, Option<T>>) -> Self {
		Self {
			slice: src.slice.iter(),
			skip_count: NonZeroUsize::new(src.from),
		}
	}
}

impl<'a, T> Iterator for FullIter<'a, T> {
	type Item = Option<&'a T>;

	fn next(&mut self) -> Option<Self::Item> {
		if let Some(old) = self.skip_count {
			// don't return anything
			self.skip_count = NonZeroUsize::new(old.get() - 1);
			Some(None)
		} else {
			self.slice.next().map(Option::as_ref)
		}
	}
}

impl<'a, T> SubArray<'a, Option<T>> {
	/// Retrieves a value at the given index, if it exists.
	///
	/// This method flattens the double layer of `Option`s that can occur when the populated
	/// slice needs to have missing values within itself.
	pub fn get_flat(&'a self, index: usize) -> Option<&'a T> {
		self.get(index).and_then(Option::as_ref)
	}
}


#[cfg(test)]
mod test {
	use super::SubArray;

	#[test]
	fn basic() {
		let sut = SubArray::new(&[1, 2, 3][..], 10);

		assert_eq!(None, sut.get(9));
		assert_eq!(Some(&1), sut.get(10));
		assert_eq!(Some(&2), sut.get(11));
		assert_eq!(Some(&3), sut.get(12));
		assert_eq!(None, sut.get(13));
	}

	#[test]
	fn empty() {
		let _: SubArray<'_, u8> = SubArray::new(&[], 0);
		let _: SubArray<'_, u8> = SubArray::new(&[], usize::MAX);
	}

	#[test]
	#[should_panic]
	fn slice_would_wrap() {
		let _ = SubArray::new(&[0, 0], usize::MAX);
	}

	#[test]
	fn critical() {
		let _ = SubArray::new(&[0, 0], usize::MAX - 1);
		let _ = SubArray::new(<&'static [u8]>::default(), usize::MAX);
	}

	#[test]
	fn full_iter() {
		let sut = SubArray::new(&[Some(1i32), Some(2), Some(3)], 2);
		let result: Vec<i32> = sut.full_iter()
			.map(|oi| oi.copied().unwrap_or(0))
			.collect();
		assert_eq!(&[0, 0, 1, 2, 3], &*result);

		let sut = SubArray::new(&[Some(5)], 0);
		assert_eq!(Some(&5), sut.full_iter().next().flatten());
	}
}