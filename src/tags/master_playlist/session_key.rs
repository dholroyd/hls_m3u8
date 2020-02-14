use std::fmt;
use std::str::FromStr;

use derive_more::{Deref, DerefMut};

use crate::types::{DecryptionKey, EncryptionMethod, ProtocolVersion};
use crate::utils::tag;
use crate::{Error, RequiredVersion};

/// # [4.3.4.5. EXT-X-SESSION-KEY]
///
/// The [`ExtXSessionKey`] tag allows encryption keys from [`MediaPlaylist`]s
/// to be specified in a [`MasterPlaylist`]. This allows the client to
/// preload these keys without having to read the [`MediaPlaylist`]s
/// first.
///
/// [`MediaPlaylist`]: crate::MediaPlaylist
/// [`MasterPlaylist`]: crate::MasterPlaylist
/// [4.3.4.5. EXT-X-SESSION-KEY]: https://tools.ietf.org/html/rfc8216#section-4.3.4.5
#[derive(Deref, DerefMut, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtXSessionKey(DecryptionKey);

impl ExtXSessionKey {
    pub(crate) const PREFIX: &'static str = "#EXT-X-SESSION-KEY:";

    /// Makes a new [`ExtXSessionKey`] tag.
    ///
    /// # Panic
    ///
    /// An [`ExtXSessionKey`] should only be used,
    /// if the segments of the stream are encrypted.
    /// Therefore this function will panic,
    /// if the `method` is [`EncryptionMethod::None`].
    ///
    /// # Example
    ///
    /// ```
    /// # use hls_m3u8::tags::ExtXSessionKey;
    /// use hls_m3u8::types::EncryptionMethod;
    ///
    /// let session_key = ExtXSessionKey::new(EncryptionMethod::Aes128, "https://www.example.com/");
    /// ```
    pub fn new<T: Into<String>>(method: EncryptionMethod, uri: T) -> Self {
        if method == EncryptionMethod::None {
            panic!("The EncryptionMethod is not allowed to be None");
        }

        Self(DecryptionKey::new(method, uri))
    }
}

/// This tag requires the same [`ProtocolVersion`] that is returned by
/// `DecryptionKey::required_version`.
impl RequiredVersion for ExtXSessionKey {
    fn required_version(&self) -> ProtocolVersion { self.0.required_version() }
}

impl fmt::Display for ExtXSessionKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.method == EncryptionMethod::None {
            // TODO: this is bad practice, this function should never fail!
            return Err(fmt::Error);
        }

        write!(f, "{}{}", Self::PREFIX, self.0)
    }
}

impl FromStr for ExtXSessionKey {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let input = tag(input, Self::PREFIX)?;
        Ok(Self(input.parse()?))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::types::{EncryptionMethod, KeyFormat};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_display() {
        let mut key = ExtXSessionKey::new(
            EncryptionMethod::Aes128,
            "https://www.example.com/hls-key/key.bin",
        );
        key.set_iv(Some([
            16, 239, 143, 117, 140, 165, 85, 17, 85, 132, 187, 91, 60, 104, 127, 82,
        ]));

        assert_eq!(
            key.to_string(),
            "#EXT-X-SESSION-KEY:METHOD=AES-128,\
             URI=\"https://www.example.com/hls-key/key.bin\",\
             IV=0x10ef8f758ca555115584bb5b3c687f52"
                .to_string()
        );
    }

    #[test]
    fn test_parser() {
        assert_eq!(
            r#"#EXT-X-SESSION-KEY:METHOD=AES-128,URI="https://priv.example.com/key.php?r=52""#
                .parse::<ExtXSessionKey>()
                .unwrap(),
            ExtXSessionKey::new(
                EncryptionMethod::Aes128,
                "https://priv.example.com/key.php?r=52"
            )
        );

        let mut key = ExtXSessionKey::new(
            EncryptionMethod::Aes128,
            "https://www.example.com/hls-key/key.bin",
        );
        key.set_iv(Some([
            16, 239, 143, 117, 140, 165, 85, 17, 85, 132, 187, 91, 60, 104, 127, 82,
        ]));

        assert_eq!(
            "#EXT-X-SESSION-KEY:METHOD=AES-128,\
             URI=\"https://www.example.com/hls-key/key.bin\",\
             IV=0X10ef8f758ca555115584bb5b3c687f52"
                .parse::<ExtXSessionKey>()
                .unwrap(),
            key
        );

        key.set_key_format(Some(KeyFormat::Identity));

        assert_eq!(
            "#EXT-X-SESSION-KEY:\
             METHOD=AES-128,\
             URI=\"https://www.example.com/hls-key/key.bin\",\
             IV=0x10ef8f758ca555115584bb5b3c687f52,\
             KEYFORMAT=\"identity\""
                .parse::<ExtXSessionKey>()
                .unwrap(),
            key
        )
    }

    #[test]
    fn test_required_version() {
        assert_eq!(
            ExtXSessionKey::new(EncryptionMethod::Aes128, "https://www.example.com/")
                .required_version(),
            ProtocolVersion::V1
        );
    }

    // ExtXSessionKey::new should panic, if the provided
    // EncryptionMethod is None!
    #[test]
    #[should_panic]
    fn test_new_panic() { ExtXSessionKey::new(EncryptionMethod::None, ""); }

    #[test]
    #[should_panic]
    fn test_display_err() {
        ExtXSessionKey(DecryptionKey::new(EncryptionMethod::None, "")).to_string();
    }

    #[test]
    fn test_deref() {
        let key = ExtXSessionKey::new(EncryptionMethod::Aes128, "https://www.example.com/");

        assert_eq!(key.method(), EncryptionMethod::Aes128);
        assert_eq!(key.uri(), Some(&"https://www.example.com/".into()));
    }

    #[test]
    fn test_deref_mut() {
        let mut key = ExtXSessionKey::new(EncryptionMethod::Aes128, "https://www.example.com/");

        key.set_method(EncryptionMethod::None);
        assert_eq!(key.method(), EncryptionMethod::None);
        key.set_uri(Some("https://www.github.com/"));
        assert_eq!(key.uri(), Some(&"https://www.github.com/".into()));
    }
}
