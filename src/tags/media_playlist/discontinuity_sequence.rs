use std::fmt;
use std::str::FromStr;

use shorthand::ShortHand;

use crate::types::ProtocolVersion;
use crate::utils::tag;
use crate::Error;
use crate::RequiredVersion;

/// Allows synchronization between different renditions of the same
/// [`VariantStream`].
///
/// [`VariantStream`]: crate::tags::VariantStream
#[derive(ShortHand, Default, Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
#[shorthand(enable(must_use))]
pub struct ExtXDiscontinuitySequence {
    /// Returns the discontinuity sequence number of
    /// the first [`MediaSegment`] that appears in the associated playlist.
    ///
    /// # Example
    ///
    /// ```
    /// # use hls_m3u8::tags::ExtXDiscontinuitySequence;
    /// let mut discontinuity_sequence = ExtXDiscontinuitySequence::new(5);
    ///
    /// discontinuity_sequence.set_seq_num(10);
    /// assert_eq!(discontinuity_sequence.seq_num(), 10);
    /// ```
    ///
    /// [`MediaSegment`]: crate::MediaSegment
    seq_num: u64,
}

impl ExtXDiscontinuitySequence {
    pub(crate) const PREFIX: &'static str = "#EXT-X-DISCONTINUITY-SEQUENCE:";

    /// Makes a new [`ExtXDiscontinuitySequence`] tag.
    ///
    /// # Example
    ///
    /// ```
    /// # use hls_m3u8::tags::ExtXDiscontinuitySequence;
    /// let discontinuity_sequence = ExtXDiscontinuitySequence::new(5);
    /// ```
    #[must_use]
    pub const fn new(seq_num: u64) -> Self { Self { seq_num } }
}

/// This tag requires [`ProtocolVersion::V1`].
impl RequiredVersion for ExtXDiscontinuitySequence {
    fn required_version(&self) -> ProtocolVersion { ProtocolVersion::V1 }
}

impl fmt::Display for ExtXDiscontinuitySequence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //
        write!(f, "{}{}", Self::PREFIX, self.seq_num)
    }
}

impl FromStr for ExtXDiscontinuitySequence {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let input = tag(input, Self::PREFIX)?;
        let seq_num = input.parse().map_err(|e| Error::parse_int(input, e))?;

        Ok(Self::new(seq_num))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_display() {
        assert_eq!(
            ExtXDiscontinuitySequence::new(123).to_string(),
            "#EXT-X-DISCONTINUITY-SEQUENCE:123".to_string()
        );
    }

    #[test]
    fn test_required_version() {
        assert_eq!(
            ExtXDiscontinuitySequence::new(123).required_version(),
            ProtocolVersion::V1
        )
    }

    #[test]
    fn test_parser() {
        assert_eq!(
            ExtXDiscontinuitySequence::new(123),
            "#EXT-X-DISCONTINUITY-SEQUENCE:123".parse().unwrap()
        );

        assert_eq!(
            ExtXDiscontinuitySequence::from_str("#EXT-X-DISCONTINUITY-SEQUENCE:12A"),
            Err(Error::parse_int("12A", "12A".parse::<u64>().expect_err("")))
        );
    }

    #[test]
    fn test_seq_num() {
        let mut sequence = ExtXDiscontinuitySequence::new(123);
        assert_eq!(sequence.seq_num(), 123);
        sequence.set_seq_num(1);
        assert_eq!(sequence.seq_num(), 1);
    }
}
