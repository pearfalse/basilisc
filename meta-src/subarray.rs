use std::{ops::Index, num::NonZeroUsize};

pub mod traits {
	pub use super::SubArrayFlat;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SubArray<'a, T> {
	slice: &'a [T],
	from: usize,
}

impl<'a, T> SubArray<'a, T> {
	pub const fn new(slice: &'a [T], from: usize) -> Self {
		// ensure top of slice is in range
		if from.checked_add(slice.len().saturating_sub(1)).is_none() {
			panic!("slice is too big for given start offset");
		}

		SubArray { slice, from }
	}

	pub fn get(&self, index: usize) -> Option<&'a T> {
		index.checked_sub(self.from).and_then(|raw_idx| self.slice.get(raw_idx))
	}

	pub fn raw_slice(&self) -> &'a [T] {
		self.slice
	}
}

impl<'a, T> SubArray<'a, Option<T>> {
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

pub struct FullIter<'a, T> {
	slice: core::slice::Iter<'a, Option<T>>,
	skip_count: Option<NonZeroUsize>,
}

impl<'a, T> FullIter<'a, T> {
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


pub trait SubArrayFlat<'a, T> {
	fn get_flat(&'a self, index: usize) -> Option<&'a T>;
}

impl<'a, T> SubArrayFlat<'a, T> for SubArray<'a, Option<T>> {
	fn get_flat(&'a self, index: usize) -> Option<&'a T> {
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