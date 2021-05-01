use crate::tags::{ExtXVersion, ExtXMediaSequence, ExtXIndependentSegments, ExtXTargetDuration, ExtXStart, ExtXByteRange, ExtXEndList, ExtXDiscontinuity, ExtXDiscontinuitySequence};
use std::time::Duration;
use crate::types::{ProtocolVersion, PlaylistType, EncryptionMethod, InitializationVector, KeyFormat, KeyFormatVersions, ByteRange, Float};
use std::borrow::Cow;
use std::str::FromStr;
use std::fmt;
use memchr::memchr;
#[cfg(feature = "chrono")]
use chrono::{DateTime, FixedOffset, SecondsFormat};
use std::collections::BTreeMap;
use crate::attribute::AttributePairs;
use crate::utils::unquote;
use std::convert::TryFrom;
use thincollections::thin_vec::ThinVec;

pub struct MyMediaPlaylist {
    pub version: ProtocolVersion,
    pub target_duration: Duration,
    pub media_sequence: usize,
    pub discontinuity_sequence: usize,
    pub playlist_type: Option<PlaylistType>,
    pub has_i_frames_only: bool,
    pub has_independent_segments: bool,
    pub start: Option<ExtXStart>,
    pub has_end_list: bool,
    segments: Vec<MediaSegData>,

    uri_text: String,
    pub allowable_excess_duration: Duration,
    pub unknown: Vec<String>,
}
impl MyMediaPlaylist {
    pub fn segments<'playlist>(&'playlist self) -> impl Iterator<Item=MyMediaSegment<'playlist>> {
        (0..self.segments.len()).into_iter().map(move |index| MyMediaSegment {
            playlist: self,
            index,
        })
    }
    pub fn last_segment(&self) -> Option<MyMediaSegment> {
        if self.segments.is_empty() {
            None
        } else {
            Some(MyMediaSegment{
                playlist: self,
                index: self.segments.len() - 1,
            })
        }
    }
}

pub struct MyMediaSegment<'playlist> {
    playlist: &'playlist MyMediaPlaylist,
    index: usize,
}
impl<'playlist> MyMediaSegment<'playlist> {
    fn seg(&self) -> &MediaSegData {
        &self.playlist.segments[self.index]
    }
    pub fn duration(&self) -> &ExtInf {
        self.seg().duration.as_ref().unwrap()
    }
    pub fn number(&self) -> usize {
        self.playlist.media_sequence + self.index
    }
    pub fn uri(&self) -> &str {
        let (from, to) = self.seg().uri.as_ref().unwrap();
        &self.playlist.uri_text[*from..*to]
    }
    pub fn keys(&self) -> &[ExtXKey] {
        self.seg().keys.as_slice()
    }
    pub fn map(&self) -> Option<&ExtXMap> {
        self.seg().map.as_ref().map(|m| m.as_ref() )
    }
    pub fn byte_range(&self) -> Option<&ExtXByteRange> {
        self.seg().byte_range.as_ref()
    }
    pub fn date_range(&self) -> Option<&ExtXDateRange> {
        self.seg().date_range.as_ref().map(|d| d.as_ref() )
    }
    pub fn has_discontinuity(&self) -> bool {
        self.seg().has_discontinuity
    }
    pub fn program_date_time(&self) -> Option<&ExtXProgramDateTime> {
        self.seg().program_date_time.as_ref()
    }
}
impl<'playlist> fmt::Debug for MyMediaSegment<'playlist> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        write!(f, "{:?}", self.playlist.segments[self.index])
    }
}

#[derive(Debug)]
pub struct MediaSegData {
    pub keys: ThinVec<ExtXKey>,
    pub map: Option<Box<ExtXMap>>,
    pub byte_range: Option<ExtXByteRange>,
    // ExtXDateRange is kept in a box because most segments are not expected to have this tag,
    // and keeping the value inline significantly increases the size of the containing segment
    pub date_range: Option<Box<ExtXDateRange>>,
    pub has_discontinuity: bool,
    pub program_date_time: Option<ExtXProgramDateTime>,
    pub duration: Option<ExtInf>,
    pub(crate) uri: Option<(usize, usize)>,
}
#[derive(Debug)]
pub struct ExtXProgramDateTime {
    /// The date-time of the first sample of the associated media segment.
    #[cfg(feature = "chrono")]
    pub date_time: DateTime<FixedOffset>,
    /// The date-time of the first sample of the associated media segment.
    #[cfg(not(feature = "chrono"))]
    pub date_time: String,
}
impl ExtXProgramDateTime {
    pub(crate) const PREFIX: &'static str = "#EXT-X-PROGRAM-DATE-TIME:";

    #[cfg(feature = "chrono")]
    pub const fn new(date_time: DateTime<FixedOffset>) -> Self {
        Self { date_time }
    }

    #[cfg(not(feature = "chrono"))]
    pub fn new<T: Into<String>>(date_time: T) -> Self {
        Self { date_time: date_time.into() }
    }
}
#[derive(Debug)]
pub struct ExtXMap {
    uri: String,
    range: Option<ByteRange>,
    pub(crate) keys: ThinVec<ExtXKey>,
}
#[derive(Debug)]
pub struct ExtXDateRange {
    pub id: String,
    pub class: Option<String>,
    #[cfg(feature = "chrono")]
    pub start_date: Option<DateTime<FixedOffset>>,
    #[cfg(not(feature = "chrono"))]
    pub start_date: Option<String>,
    #[cfg(feature = "chrono")]
    pub end_date: Option<DateTime<FixedOffset>>,
    #[cfg(not(feature = "chrono"))]
    pub end_date: Option<String>,
    pub duration: Option<Duration>,
    pub planned_duration: Option<Duration>,
    pub scte35_cmd: Option<String>,
    pub scte35_out: Option<String>,
    pub scte35_in: Option<String>,
    pub end_on_next: bool,
    pub client_attributes: BTreeMap<String, Value>,
}
impl ExtXDateRange {
    pub fn from_str_attrs(input: &str, span: Span) -> std::result::Result<Self, ParseError> {
        let mut id = None;
        let mut class = None;
        let mut start_date = None;
        let mut end_date = None;
        let mut duration = None;
        let mut planned_duration = None;
        let mut scte35_cmd = None;
        let mut scte35_out = None;
        let mut scte35_in = None;
        let mut end_on_next = false;

        let mut client_attributes = BTreeMap::new();

        for (key, value) in AttributePairs::new(input) {
            match key {
                "ID" => id = Some(unquote(value)),
                "CLASS" => class = Some(unquote(value)),
                "START-DATE" => {
                    #[cfg(feature = "chrono")]
                        {
                            start_date = Some(unquote(value).parse().map_err(ParseError::chrono)?)
                        }
                    #[cfg(not(feature = "chrono"))]
                        {
                            start_date = Some(unquote(value).to_string())
                        }
                }
                "END-DATE" => {
                    #[cfg(feature = "chrono")]
                        {
                            end_date = Some(unquote(value).parse().map_err(ParseError::chrono)?)
                        }
                    #[cfg(not(feature = "chrono"))]
                        {
                            end_date = Some(unquote(value).to_string())
                        }
                }
                "DURATION" => {
                    duration = Some(Duration::from_secs_f64(
                        value.parse().map_err(|e| ParseError::ParseFloatError{input: value.to_string(), source:e})?,
                    ));
                }
                "PLANNED-DURATION" => {
                    planned_duration = Some(Duration::from_secs_f64(
                        value.parse().map_err(|e| ParseError::ParseFloatError{input:value.to_string(), source:e})?,
                    ));
                }
                "SCTE35-CMD" => scte35_cmd = Some(unquote(value)),
                "SCTE35-OUT" => scte35_out = Some(unquote(value)),
                "SCTE35-IN" => scte35_in = Some(unquote(value)),
                "END-ON-NEXT" => {
                    if value != "YES" {
                        return Err(ParseError::Unexpected {
                            expected: b"YES",
                            at: span,  // TODO: calculate sub-span
                        });
                    }
                    end_on_next = true;
                }
                _ => {
                    if key.starts_with("X-") {
                        if key.chars().any(|c| {
                            c.is_ascii_lowercase()
                                || !c.is_ascii()
                                || !(c.is_alphanumeric() || c == '-')
                        }) {
                            eprintln!("TODO: a client attribute can only consist of uppercase ascii characters, numbers or `-`");
                            return Err(ParseError::Attributes);
                        }

                        client_attributes.insert(key.to_owned(), Value::try_from(value)?);
                    } else {
                        // [6.3.1. General Client Responsibilities]
                        // > ignore any attribute/value pair with an
                        // unrecognized AttributeName.
                    }
                }
            }
        }

        let id = id.ok_or_else(|| ParseError::missing_attribute("ID", span))?;
        let id = id.into_owned();

        if end_on_next && class.is_none() {
            return Err(ParseError::missing_attribute("CLASS", span));
        } else if end_on_next && duration.is_some() {
            return Err(ParseError::unexpected_attribute("DURATION", span));
        } else if end_on_next && end_date.is_some() {
            return Err(ParseError::unexpected_attribute("END-DATE", span));
        }

        // TODO: verify this without chrono?
        // https://tools.ietf.org/html/rfc8216#section-4.3.2.7
        #[cfg(feature = "chrono")]
            {
                if let (Some(start_date), Some(Ok(duration)), Some(end_date)) = (
                    start_date,
                    duration.map(chrono::Duration::from_std),
                    &end_date,
                ) {
                    if start_date + duration != *end_date {
                        // TODO
                        //return Err(Error::custom(
                        //    "end_date must be equal to start_date + duration",
                        //));
                    }
                }
            }

        Ok(Self {
            id,
            class: class.map(|s| s.to_string() ),
            start_date,
            end_date,
            duration,
            planned_duration,
            scte35_cmd: scte35_cmd.map(|s| s.to_string() ),
            scte35_out: scte35_out.map(|s| s.to_string() ),
            scte35_in: scte35_in.map(|s| s.to_string() ),
            end_on_next,
            client_attributes,
        })
    }
}
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Value {
    String(String),
    Hex(Vec<u8>),
    Float(Float),
}
impl<'a> TryFrom<&'a str> for Value {
    type Error = ParseError;

    fn try_from(input: &'a str) -> std::result::Result<Self, ParseError> {
        if input.starts_with("0x") || input.starts_with("0X") {
            Ok(Self::Hex(
                hex::decode(input.trim_start_matches("0x").trim_start_matches("0X"))
                    .map_err(ParseError::hex)?,
            ))
        } else {
            match input.parse() {
                Ok(value) => Ok(Self::Float(value)),
                Err(_) => Ok(Self::String(unquote(input).to_string())),
            }
        }
    }
}
#[derive(Debug, PartialEq)]
pub struct ExtInf {
    duration: Duration,
    title: Option<String>,
}
impl ExtInf {
    fn new(duration: Duration) -> ExtInf {
        ExtInf {
            duration,
            title: None,
        }
    }
    pub fn duration(&self) -> Duration {
        self.duration
    }
}

#[derive(Debug)]
pub struct ExtXKey(pub Option<DecryptionKey>);
#[derive(Debug)]
pub struct DecryptionKey {
    pub method: EncryptionMethod,
    pub(crate) uri: String,
    pub iv: InitializationVector,
    pub format: Option<KeyFormat>,
    pub versions: Option<KeyFormatVersions>,
}

struct LazyStr<'a>(&'a[u8]);
impl<'a> LazyStr<'a> {
    fn try_to_string(&self) -> std::result::Result<String, std::string::FromUtf8Error> {
        String::from_utf8(Vec::from(self.0))
    }
    fn try_str(&self) -> std::result::Result<Cow<'a, str>, std::str::Utf8Error> {
        std::str::from_utf8(self.0).map(Cow::Borrowed)
    }
}
impl<'a> fmt::Debug for LazyStr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(String::from_utf8_lossy(self.0).as_ref())
    }
}

// TODO: fold into crate::Error
#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    Incomplete { element_name: &'static str, at: Pos },
    Unexpected { expected: &'static [u8], at: Span },
    Utf8(Vec<u8>, Span),
    InvalidNumber,
    ExpectedEndOfInput(String),
    Attributes,
    #[cfg(feature = "chrono")]
    Chrono(chrono::ParseError),
    ParseFloatError {
        input: String,
        source: ::std::num::ParseFloatError,
    },
    MissingAttribute {
        name: &'static str,
        at: Span,
    },
    UnexpectedAttribute {
        name: &'static str,
        at: Span,
    },
    Hex { source: hex::FromHexError },
    MissingTargetDuration,
    MissingVersion,
    UrlWithoutExtinf { url: String, at: Span },
    /// TODO: remove this variant from the public interface
    PeekFailed,
}
impl ParseError {
    fn missing_attribute(name: &'static str, at: Span) -> Self {
        Self::MissingAttribute { name, at }
    }
    fn unexpected_attribute(name: &'static str, at: Span) -> Self {
        Self::UnexpectedAttribute { name, at }
    }
    fn hex(source: hex::FromHexError) -> Self {
        Self::Hex { source }
    }
    #[cfg(feature = "chrono")]
    fn chrono(source: chrono::ParseError) -> Self {
        Self::Chrono(source)
    }
}
type Result<'a, T> = std::result::Result<(&'a [u8], T), ParseError>;

enum SegmentState {
    AwaitExtinf,
    InSegment,
}

// TODO: reconcile with crate::MediaSegmentBuilder
pub struct MySegmentBuilder {
    state: SegmentState,
    pub(crate) segments: Vec<MediaSegData>,
    uri_text: String,
    program_date_time: Option<ExtXProgramDateTime>,
    date_range: Option<ExtXDateRange>,
    ext_inf: Option<ExtInf>,
}
fn push_placeholder(segments: &mut Vec<MediaSegData>) {
    segments.push(MediaSegData {
        keys: thincollections::thinvec![],
        map: None,
        byte_range: None,
        date_range: None,
        has_discontinuity: false,
        program_date_time: None,
        duration: None,
        uri: None
    });
}
impl<'a> Default for MySegmentBuilder {
    fn default() -> Self {
        let mut segments = Vec::default();
        push_placeholder(&mut segments);
        MySegmentBuilder {
            state: SegmentState::AwaitExtinf,
            segments,
            uri_text: String::new(),
            program_date_time: None,
            date_range: None,
            ext_inf: None,
        }
    }
}
impl MySegmentBuilder {
    fn placeholder_mut(&mut self) -> &mut MediaSegData {
        self.segments.last_mut().unwrap()
    }
    fn uri(&mut self, uri: LazyStr<'_>, span: Span) -> std::result::Result<(), ParseError> {
        let uri = match uri.try_str() {
            Ok(v) => v,
            Err(e) => {
                return Err(ParseError::Utf8(uri.0.to_vec(), span));
            },
        };
        match self.state {
            SegmentState::AwaitExtinf => {
                return Err(ParseError::UrlWithoutExtinf {
                    url: uri.to_string(),
                    at: span,
                });
            },
            SegmentState::InSegment => {
                let from = self.uri_text.len();
                self.uri_text.push_str(uri.as_ref());
                let to = self.uri_text.len();
                let placeholder = self.placeholder_mut();
                placeholder.uri = Some((from, to));
                push_placeholder(&mut self.segments);
            },
        }
        self.state = SegmentState::AwaitExtinf;
        Ok(())
    }
    fn date_range(&mut self, date_range: ExtXDateRange) {
        self.placeholder_mut().date_range = Some(Box::new(date_range));
    }

    fn program_date_time(&mut self, date_time: ExtXProgramDateTime) {
        self.placeholder_mut().program_date_time = Some(date_time);
    }

    fn ext_inf<'a>(&mut self, duration: Duration, title: Option<LazyStr<'a>>) {
        self.placeholder_mut().duration = Some(ExtInf::new(duration));
        self.state = SegmentState::InSegment;
    }

    fn discontinuity(&mut self) {
        self.placeholder_mut().has_discontinuity = true;
    }

}

// TODO: reconcile with crate::MediaPlaylistBuilder
pub struct MyPlaylistBuilder {
    version: Option<ProtocolVersion>,
    media_sequence: Option<usize>,
    discontinuity_sequence: Option<usize>,
    target_duration: Option<Duration>,
    independent_segments: bool,
    endlist: bool,
    segment_builder: MySegmentBuilder,
    errors: Vec<ParseError>,
}
impl Default for MyPlaylistBuilder {
    fn default() -> Self {
        MyPlaylistBuilder {
            version: None,
            media_sequence: None,
            discontinuity_sequence: None,
            target_duration: None,
            independent_segments: false,
            endlist: false,
            segment_builder: Default::default(),
            errors: vec![]
        }
    }
}
impl<'a> MyPlaylistBuilder {
    fn add_target_duration(&mut self, d: ExtXTargetDuration) {
        if let Some(old_v) = self.target_duration {
            // TODO: emit event
            println!("EXT-X-TARGETDURATION redefined");
        }
        self.target_duration = Some(d.0)
    }

    fn add_independent_segments(&mut self, _independent_segments: ExtXIndependentSegments) {
        self.independent_segments = true;
    }

    fn add_endlist(&mut self, _endlist: ExtXEndList) {
        self.endlist = true;
    }

    fn add_media_sequence_number(&mut self, seq: ExtXMediaSequence) {
        if let Some(msn) = self.media_sequence {
            // TODO: emit event
            println!("EXT-X-MEDIA-SEQUENCE redefined; current {}, duplicate {}", msn, seq.0);
        } else {
            self.media_sequence = Some(seq.0);
        }
    }

    fn add_version(&mut self, v: ExtXVersion) {
        if let Some(old_v) = self.version {
            // TODO: emit event
            println!("EXT-X-VERSION redefined; was {}, now {}", old_v, v.version());
        }
        self.version = Some(v.version())
    }

    fn add_program_date_time(&mut self, datetime: ExtXProgramDateTime) {
        self.segment_builder.program_date_time(datetime);
    }

    fn add_date_range(&mut self, daterange: ExtXDateRange) {
        self.segment_builder.date_range(daterange);
    }

    fn add_duration(&mut self, duration: Duration, title: Option<LazyStr<'a>>) {
        self.segment_builder.ext_inf(duration, title);
    }

    fn add_discontinuity_sequence(&mut self, disco_seq: ExtXDiscontinuitySequence) {
        self.discontinuity_sequence = Some(disco_seq.0);
    }

    fn add_discontinuity(&mut self, disco: ExtXDiscontinuity) {
        self.segment_builder.discontinuity();
    }

    fn add_uri(&mut self, uri: LazyStr<'a>, span: Span) {
        if let Err(e) = self.segment_builder.uri(uri, span) {
            self.err(e);
        }
    }

    fn add_comment(&mut self, _comment: LazyStr<'a>) {
        // ignore comments
    }

    fn add_blank(&mut self) {
        // ignore blank lines
    }

    pub fn build(self) -> std::result::Result<MyMediaPlaylist, ParseError> {
        let mut segments = self.segment_builder.segments;
        let _ = segments.pop();
        segments.shrink_to_fit();
        Ok(MyMediaPlaylist {
            version: self.version.map(|v| v ).ok_or_else(|| ParseError::MissingVersion)?,
            target_duration: self.target_duration.ok_or_else(|| ParseError::MissingTargetDuration)?,
            media_sequence: self.media_sequence.unwrap_or(0),
            discontinuity_sequence: self.discontinuity_sequence.unwrap_or(0),
            playlist_type: None,
            has_i_frames_only: false,  // TODO
            has_independent_segments: self.independent_segments,
            start: None,
            has_end_list: self.endlist,
            segments,
            uri_text: self.segment_builder.uri_text,
            allowable_excess_duration: Default::default(),
            unknown: vec![]
        })
    }

    fn err(&mut self, err: ParseError) {
        self.errors.push(err);
    }

    pub fn errors(&self) -> impl Iterator<Item=&ParseError> {
        self.errors.iter()
    }
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}
impl Span {
    pub fn from_start_end(start: Pos, end: Pos) -> Span {
        assert!(start <= end, "start={:?} <= end={:?}", start, end);
        Span {
            start,
            end,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Pos(pub usize);

pub struct Cursor<'a> {
    buf: &'a [u8],
    pos: usize,
}
impl<'a> From<&'a [u8]> for Cursor<'a> {
    fn from(buf: &'a [u8]) -> Self {
        Cursor {
            buf,
            pos: 0,
        }
    }
}
impl<'a> Cursor<'a> {
    pub fn take(&mut self, n: usize) -> std::result::Result<&'a[u8], ParseError> {
        if self.buf.len() < n {
            return Err(ParseError::Incomplete{ element_name: "take", at: self.pos() })  // FIXME: this param to Incomplete makes no sense
        }
        self.pos += n;
        let (head, tail) = self.buf.split_at(n);
        self.buf = tail;
        Ok(head)
    }

    pub fn tag(&mut self, expected: &'static [u8]) -> std::result::Result<(), ParseError> {
        if self.buf.starts_with(expected) {
            self.pos += expected.len();
            self.buf = &self.buf[expected.len()..];
            Ok(())
        } else {
            let (found, at) = self.peek_till_eol();
            let e = ParseError::Unexpected { expected, at };
            Err(e)
        }
    }

    pub fn peek_tag(&mut self, expected: &'static [u8]) -> std::result::Result<(), ParseError> {
        if self.buf.starts_with(expected) {
            self.pos += expected.len();
            self.buf = &self.buf[expected.len()..];
            Ok(())
        } else {
            Err(ParseError::PeekFailed)
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn into_buf(self) -> &'a [u8] {
        self.buf
    }

    fn peek_till_eol(&self) -> (&'a[u8], Span) {
        if let Some(pos) = memchr(b'\n', self.buf) {
            (&self.buf[..pos], Span::from_start_end(self.pos(), Pos(self.pos + pos)))
        } else {
            (self.buf, Span::from_start_end(self.pos(), Pos(self.pos + self.buf.len())))
        }
    }
    fn take_till_eol(&mut self) -> (&'a[u8], Span) {
        if let Some(pos) = memchr(b'\n', self.buf) {
            let (head, tail) = self.buf.split_at(pos);
            let span = Span::from_start_end(self.pos(), Pos(self.pos + pos));
            self.buf = tail;
            self.pos += pos;
            (head, span)
        } else {
            let res = self.buf.clone();
            let span = Span::from_start_end(self.pos(), Pos(self.pos + self.buf.len()));
            self.pos += self.buf.len();
            self.buf = &self.buf[0..0];
            (res, span)
        }
    }

    fn u64(&mut self) -> std::result::Result<u64, ParseError> {
        match ::lexical_core::parse_partial(self.buf) {
            Ok((value, processed)) => {
                self.take(processed);
                Ok(value)
            },
            Err(_) => Err(ParseError::InvalidNumber)
        }
    }

    fn f64(&mut self) -> std::result::Result<f64, ParseError> {

        match ::fast_float::parse_partial::<f64, _>(self.buf) {
            Ok((value, processed)) => {
                self.take(processed);
                if value.is_infinite() || value.is_nan() {
                    Err(ParseError::InvalidNumber)
                } else {
                    Ok(value)
                }
            },
            Err(_) => Err(ParseError::InvalidNumber)
        }
    }

    pub fn span_since(&self, start: Pos) -> Span {
        Span::from_start_end(start, self.pos())
    }

    fn pos(&self) -> Pos {
        Pos(self.pos)
    }
}

pub struct Parser<'a> {
    cursor: Cursor<'a>,
    builder: MyPlaylistBuilder,
}
impl<'a> Parser<'a> {
    pub fn new(cursor: Cursor<'a>) -> Parser<'a> {
        Parser {
            cursor,
            builder: MyPlaylistBuilder::default(),
        }
    }
    pub fn parse(mut self) -> std::result::Result<MyPlaylistBuilder, ParseError> {
        let this = &mut self;
        this.cursor.tag(b"#EXTM3U")?;
        this.line_ending()?;
        this.lines()?;
        if !this.cursor.is_empty() {
            return Err(ParseError::ExpectedEndOfInput(String::from_utf8_lossy(this.cursor.buf).to_string()))
        }
        Ok(self.builder)
    }
    fn lines(&mut self) -> std::result::Result<(), ParseError> {
        loop {
            if self.cursor.is_empty() {
                return Ok(());
            }
            self.hls_line()?;
        }
    }

    fn hls_line(&mut self) -> std::result::Result<(), ParseError> {
        let res = self.comment_or_tag();
        if let Ok(res) = res { return Ok(res); }

        let res = self.uri_line();
        if res.is_ok() { return res; }

        self.line_ending()?;
        self.builder.add_blank();  // TODO: represent blank line?
        Ok(())
    }

    fn comment_or_tag(&mut self) -> std::result::Result<(), ParseError> {
        self.cursor.peek_tag(b"#")?;

        let res = self.ext_tag_eol();
        if let Ok(res) = res { return Ok(res); }

        self.comment()
    }

    fn ext_tag_eol(&mut self) -> std::result::Result<(), ParseError> {
        let res = self.ext_tag()?;

        let _optional = self.line_ending();
        Ok(())
    }
    fn ext_tag(&mut self) -> std::result::Result<(), ParseError> {
        self.cursor.peek_tag(b"EXT")?;

        if let Ok((duration, title)) = self.ext_inf() {
            self.builder.add_duration(duration, title);
            return Ok(());
        }
        if let Ok(data_range) = self.ext_date_range() {
            self.builder.add_date_range(data_range);
            return Ok(());
        }
        if let Ok(date_time) = self.ext_program_date_time() {
            self.builder.add_program_date_time(date_time);
            return Ok(());
        }
        if let Ok(version) = self.ext_version() {
            self.builder.add_version(version);
            return Ok(());
        }
        if let Ok(msn) = self.ext_media_sequence() {
            self.builder.add_media_sequence_number(msn);
            return Ok(());
        }
        if let Ok(independent) = self.ext_independent_segments() {
            self.builder.add_independent_segments(independent);
            return Ok(());
        }
        if let Ok(disco) = self.ext_discontinuity_sequence() {
            self.builder.add_discontinuity_sequence(disco);
            return Ok(());
        }
        if let Ok(disco) = self.ext_discontinuity() {
            self.builder.add_discontinuity(disco);
            return Ok(());
        }
        if let Ok(endlist) = self.ext_endlist() {
            self.builder.add_endlist(endlist);
            return Ok(());
        }
        match self.ext_target_duration() {
            Ok(duration) => {
                self.builder.add_target_duration(duration);
                return Ok(())
            },
            Err(e) => Err(e),
        }
    }

    fn ext_version(&mut self) -> std::result::Result<ExtXVersion, ParseError> {
        self.cursor.tag(b"-X-VERSION:")?;
        let (val, span) = self.cursor.take_till_eol();
        Ok(ExtXVersion::new(ProtocolVersion::from_str(utf8(val, span)?).map_err(|_| ParseError::Attributes)?))
    }

    fn ext_media_sequence(&mut self) -> std::result::Result<ExtXMediaSequence, ParseError> {
        self.cursor.tag(b"-X-MEDIA-SEQUENCE:")?;
        let msn = self.cursor.u64()?;
        Ok(ExtXMediaSequence(msn as usize))
    }

    fn ext_independent_segments(&mut self) -> std::result::Result<ExtXIndependentSegments, ParseError> {
        self.cursor.tag(b"-X-INDEPENDENT-SEGMENTS")?;
        Ok(ExtXIndependentSegments)
    }

    fn ext_discontinuity_sequence(&mut self) -> std::result::Result<ExtXDiscontinuitySequence, ParseError> {
        self.cursor.tag(b"-X-DISCONTINUITY-SEQUENCE:")?;
        let seq = self.cursor.u64()?;
        Ok(ExtXDiscontinuitySequence(seq as usize))
    }

    fn ext_discontinuity(&mut self) -> std::result::Result<ExtXDiscontinuity, ParseError> {
        self.cursor.tag(b"-X-DISCONTINUITY")?;
        Ok(ExtXDiscontinuity)
    }

    fn ext_target_duration(&mut self) -> std::result::Result<ExtXTargetDuration, ParseError> {
        self.cursor.tag(b"-X-TARGETDURATION:")?;
        let val = self.cursor.u64()?;
        Ok(ExtXTargetDuration(std::time::Duration::from_secs(val)))
    }

    fn ext_endlist(&mut self) -> std::result::Result<ExtXEndList, ParseError> {
        self.cursor.tag(b"-X-ENDLIST")?;
        Ok(ExtXEndList)
    }


    fn ext_program_date_time(&mut self) -> std::result::Result<ExtXProgramDateTime, ParseError> {
        self.cursor.tag(b"-X-PROGRAM-DATE-TIME:")?;
        let (val, span) = self.cursor.take_till_eol();
        utf8(val, span).and_then(|v| {
            #[cfg(feature = "chrono")] {
                Ok(ExtXProgramDateTime::new(chrono::DateTime::parse_from_rfc3339(v).map_err(ParseError::Chrono)?))
            }
            #[cfg(not(feature = "chrono"))] {
                Ok(ExtXProgramDateTime::new(v))
            }
        })
    }

    fn ext_date_range(&mut self) -> std::result::Result<ExtXDateRange, ParseError> {
        self.cursor.peek_tag(b"-X-DATERANGE:")?;
        let (val, span) = self.cursor.take_till_eol();
        utf8(val, span).and_then(|s| ExtXDateRange::from_str_attrs(s, span).map_err(|_| ParseError::Attributes))
    }

    fn comment(&mut self) -> std::result::Result<(), ParseError> {
        let (res, span) = self.cursor.take_till_eol();
        let res = LazyStr(res);

        let _optional = self.line_ending();
        self.builder.add_comment(res);
        Ok(())
    }

    fn ext_inf(&mut self) -> std::result::Result<(Duration, Option<LazyStr<'a>>), ParseError> {
        self.cursor.peek_tag(b"INF:")?;
        let duration = self.duration()?;
        self.cursor.tag(b",")?;
        let description = self.description()?;

        Ok((duration, description))
    }

    fn uri_line(&mut self) -> std::result::Result<(), ParseError> {
        if self.cursor.is_empty() {
            return Err(ParseError::Incomplete { element_name: "uri", at: self.cursor.pos() })
        }
        let (res, span) = self.cursor.take_till_eol();
        if res.is_empty() {
            return Err(ParseError::Incomplete { element_name: "uri", at: self.cursor.pos() })
        }
        let res = if res.ends_with(b"\r") {
            &res[..res.len() - 1]
        } else {
            res
        };
        let line = LazyStr(res);
        let _optional = self.line_ending();
        self.builder.add_uri(line, span);
        Ok(())
    }


    fn duration(&mut self) -> std::result::Result<Duration, ParseError> {
        self.cursor.f64()
            .map(Duration::from_secs_f64)
    }
    fn description(&mut self) -> std::result::Result<Option<LazyStr<'a>>, ParseError> {
        let (r, span) = self.cursor.take_till_eol();
        if r.is_empty() {
            Ok(None)
        } else {
            Ok(Some(LazyStr(r)))
        }
    }

    fn line_ending(&mut self) -> std::result::Result<(), ParseError> {
        if self.cursor.is_empty() {
            Err(ParseError::Incomplete { element_name: "line-end", at: self.cursor.pos() } )
        } else {
            if let Ok(_) = self.cursor.peek_tag(b"\r\n") {
                Ok(())
            } else {
                self.cursor.tag(b"\n")
            }
        }
    }
}
fn utf8(input: &[u8], span: Span) -> std::result::Result<&str, ParseError> {
    std::str::from_utf8(input)
        .map_err(|_| ParseError::Utf8(input.to_vec(), span))
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bad_magic() {
        println!("size MediaSegData = {}", std::mem::size_of::<MediaSegData>());
        println!("size Vec = {}", std::mem::size_of::<Vec<ExtXKey>>());
        println!("size ExtXMap = {}", std::mem::size_of::<ExtXMap>());
        println!("size ExtInf = {}", std::mem::size_of::<ExtInf>());
        println!("size Option<ExtXByteRange> = {}", std::mem::size_of::<Option<ExtXByteRange>>());
        let data = b"foo\nbar";
        let p = Parser::new(data.as_ref().into());
        let b = p.parse();
        assert_eq!(
            b.err(),
            Some(ParseError::Unexpected {
                expected: b"#EXTM3U",
                at: Span { start: Pos(0), end: Pos(3) }
            })
        );
    }
}
