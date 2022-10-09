use std::num::NonZeroU16;

use nonzero_ext::nonzero;

#[derive(Debug, PartialEq, PartialOrd, Clone, thiserror::Error)]
enum Error {
	#[error("too many lines to fully number the program (had room for {max_possible}, but found {found}")]
	TooManyUnnumberedLines { max_possible: u16, found: u32 },
}

type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Range {
	pub start: u16,
	pub step: NonZeroU16,
}

impl Range {
	#[inline]
	fn new(start: u16, step: NonZeroU16) -> Self {
		Self { start, step }
	}
}

static INFERENCE_ALIGNMENT_TRIES: [(u16, NonZeroU16); 4] = [
	(10, nonzero!(10u16)),
	(5, nonzero!(10u16)),
	(5, nonzero!(5u16)),
	(1, nonzero!(1u16)),
];

fn infer_line_number_range(line_before: Option<u16>, line_after: Option<u16>, num_lines: u16)
-> Result<Range> {
	debug_assert!(line_before.or(line_after).is_some());
	let line_before = line_before.unwrap_or(0);
	let line_after = line_after.unwrap_or(0xff00);
	debug_assert!(line_before < line_after);

	let ctor = move |start: u16, step: NonZeroU16| Ok(Range::new(start, step));

	// try various heuristics, in very subjective order of niceness
	for (align, step) in INFERENCE_ALIGNMENT_TRIES {
		let aligned_first = (line_before + align) / align * align;
		let aligned_last = aligned_first + ((num_lines - 1) * step.get());
		if aligned_last < line_after {
			return ctor(aligned_first, step);
		}
	}

	Err(Error::TooManyUnnumberedLines {
		max_possible: line_after - line_before,
		found: num_lines.into(),
	})
}

#[cfg(test)]
mod test_line_number_inference {
	use super::*;

	fn range(begin: u16, step: u16) -> Result<Range> {
		Ok(Range::new(begin, NonZeroU16::new(step).unwrap()))
	}

	#[test]
	fn absolute() {
		assert_eq!(range(10, 10), infer_line_number_range(Some(5), Some(110), 10));
		assert_eq!(range(5, 10), infer_line_number_range(Some(4), Some(26), 3));
		assert_eq!(range(6, 1), infer_line_number_range(Some(5), Some(11), 5));
	}

	#[test]
	fn absolute_spacious() {
		assert_eq!(range(10, 10), infer_line_number_range(Some(5), Some(2000), 5));
	}

	#[test]
	fn step_1() {
		assert_eq!(range(11, 1), infer_line_number_range(Some(10), Some(31), 20));
	}
}
