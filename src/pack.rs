use std::num::NonZeroU16;

use nonzero_ext::nonzero;

#[derive(Debug, PartialEq, PartialOrd, Clone, thiserror::Error)]
enum Error {
	#[error("too many lines to fully number the program (had room for {max_possible}, but needed {needed}")]
	TooManyUnnumberedLines { max_possible: u16, needed: u32 },
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
	let line_after = line_after.unwrap_or(0xff00);
	debug_assert!(line_before.unwrap_or(0) < line_after);

	let ctor = move |start: u16, step: NonZeroU16| Ok(Range::new(start, step));

	// try various heuristics, in very subjective order of niceness
	for (align, step) in INFERENCE_ALIGNMENT_TRIES {
		let line_before = line_before.unwrap_or_else(|| match step.get() {
			1 => 0,
			_ => 1,
		});
		let aligned_first = (line_before + align) / align * align;
		let aligned_last = aligned_first + ((num_lines - 1) * step.get());
		if aligned_last < line_after {
			return ctor(aligned_first, step);
		}
	}

	Err(Error::TooManyUnnumberedLines {
		max_possible: line_after - line_before.unwrap_or(0) - 1,
		needed: num_lines.into(),
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
		fn case(exp_start: u16, exp_step: u16, line_before: u16, line_after: u16, num_lines: u16) {
			let expected = Range::new(exp_start, NonZeroU16::new(exp_step).unwrap());
			assert_eq!(Ok(expected), infer_line_number_range(
				Some(line_before), Some(line_after), num_lines));
		}
		case(10, 10, 5, 110, 10);
		case(5, 10, 4, 26, 3);
		case(6, 1, 5, 11, 5);
	}

	#[test]
	fn absolute_spacious() {
		assert_eq!(range(10, 10), infer_line_number_range(Some(5), Some(2000), 5));
	}

	#[test]
	fn unnumbered_start() {
		assert_eq!(range(10, 10), infer_line_number_range(None, Some(200), 19));
	}

	#[test]
	fn no_room() {
		assert_eq!(
			Err(Error::TooManyUnnumberedLines { max_possible: 10, needed: 11, }),
			infer_line_number_range(Some(100), Some(111), 11)
		);

		assert_eq!(
			Err(Error::TooManyUnnumberedLines { max_possible: 0, needed: 1 }),
			infer_line_number_range(Some(1), Some(2), 1)
		);

		assert_eq!(
			Err(Error::TooManyUnnumberedLines { max_possible: 100, needed: 101 }),
			infer_line_number_range(None, Some(101), 101)
		);

		assert_eq!(
			Err(Error::TooManyUnnumberedLines { max_possible: 255, needed: 256 }),
			infer_line_number_range(Some(0xfe00), None, 256)
		);
	}
}
