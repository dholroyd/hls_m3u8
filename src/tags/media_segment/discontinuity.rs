use std::fmt;
use std::str::FromStr;

use crate::types::ProtocolVersion;
use crate::utils::tag;
use crate::{Error, RequiredVersion};

/// The `ExtXDiscontinuity` tag indicates a discontinuity between the
/// `MediaSegment` that follows it and the one that preceded it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct ExtXDiscontinuity;

impl ExtXDiscontinuity {
    pub(crate) const PREFIX: &'static str = "#EXT-X-DISCONTINUITY";
}

/// This tag requires [`ProtocolVersion::V1`].
impl RequiredVersion for ExtXDiscontinuity {
    fn required_version(&self) -> ProtocolVersion { ProtocolVersion::V1 }
}

impl fmt::Display for ExtXDiscontinuity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { Self::PREFIX.fmt(f) }
}

impl FromStr for ExtXDiscontinuity {
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
            ExtXDiscontinuity.to_string(),
            "#EXT-X-DISCONTINUITY".to_string(),
        )
    }

    #[test]
    fn test_parser() { assert_eq!(ExtXDiscontinuity, "#EXT-X-DISCONTINUITY".parse().unwrap()) }

    #[test]
    fn test_required_version() {
        assert_eq!(ExtXDiscontinuity.required_version(), ProtocolVersion::V1)
    }
}
