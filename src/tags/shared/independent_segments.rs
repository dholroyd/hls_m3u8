use std::fmt;
use std::str::FromStr;

use crate::types::ProtocolVersion;
use crate::utils::tag;
use crate::{Error, RequiredVersion};

/// Signals that all media samples in a [`MediaSegment`] can be decoded without
/// information from other segments.
///
/// [`MediaSegment`]: crate::MediaSegment
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub(crate) struct ExtXIndependentSegments;

impl ExtXIndependentSegments {
    pub(crate) const PREFIX: &'static str = "#EXT-X-INDEPENDENT-SEGMENTS";
}

/// This tag requires [`ProtocolVersion::V1`].
impl RequiredVersion for ExtXIndependentSegments {
    fn required_version(&self) -> ProtocolVersion { ProtocolVersion::V1 }
}

impl fmt::Display for ExtXIndependentSegments {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { Self::PREFIX.fmt(f) }
}

impl FromStr for ExtXIndependentSegments {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        tag(input, Self::PREFIX)?;
        Ok(Self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_display() {
        assert_eq!(
            ExtXIndependentSegments.to_string(),
            "#EXT-X-INDEPENDENT-SEGMENTS".to_string(),
        )
    }

    #[test]
    fn test_parser() {
        assert_eq!(
            ExtXIndependentSegments,
            "#EXT-X-INDEPENDENT-SEGMENTS".parse().unwrap(),
        )
    }

    #[test]
    fn test_required_version() {
        assert_eq!(
            ExtXIndependentSegments.required_version(),
            ProtocolVersion::V1
        )
    }
}
