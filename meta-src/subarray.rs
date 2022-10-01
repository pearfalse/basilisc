#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SubArray<'a, T> {
	slice: &'a [T],
	from: usize,
}

impl<'a, T> SubArray<'a, T> {
	pub fn new(slice: &'a [T], from: usize) -> Self {
		// ensure top of slice is in range
		if from.checked_add(slice.len().saturating_sub(1)).is_none() {
			panic!("slice of length {} is too big for start offset {}", slice.len(), from);
		}

		SubArray { slice, from }
	}

	pub fn get(&self, index: usize) -> Option<&'a T> {
		index.checked_sub(self.from).and_then(|raw_idx| self.slice.get(raw_idx))
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
}