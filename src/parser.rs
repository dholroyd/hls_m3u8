use crate::{MediaPlaylist, Error, MediaSegment, tags};
use crate::tags::{ExtInf, ExtXVersion, ExtXMediaSequence, ExtXIndependentSegments, ExtXTargetDuration, ExtXProgramDateTime, ExtXDateRange};
use std::time::Duration;
use crate::types::{ProtocolVersion, PlaylistType};
use std::convert::TryInto;
use std::borrow::Cow;
use std::string::FromUtf8Error;
use std::str::FromStr;
use std::ops::RangeFrom;
use std::fmt;
use memchr::memchr;
use std::io::BufRead;
use stable_vec::StableVec;

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

enum Line<'a> {
    Tag(Tag<'a>),
    Comment(LazyStr<'a>),
    Uri(LazyStr<'a>),
}
#[derive(Debug)]
enum Tag<'a> {
    ExtXVersion(tags::ExtXVersion),
    ExtInf { duration: Duration, title: Option<LazyStr<'a>> },
    ExtXByteRange(tags::ExtXByteRange),
    ExtXDiscontinuity(tags::ExtXDiscontinuity),
    ExtXKey(tags::ExtXKey<'a>),
    ExtXMap(tags::ExtXMap<'a>),
    ExtXProgramDateTime(tags::ExtXProgramDateTime<'a>),
    ExtXDateRange(tags::ExtXDateRange<'a>),
    ExtXTargetDuration(tags::ExtXTargetDuration),
    ExtXMediaSequence(tags::ExtXMediaSequence),
    ExtXDiscontinuitySequence(tags::ExtXDiscontinuitySequence),
    ExtXEndList(tags::ExtXEndList),
    PlaylistType(PlaylistType),
    ExtXIFramesOnly(tags::ExtXIFramesOnly),
    ExtXMedia(tags::ExtXMedia<'a>),
    ExtXSessionData(tags::ExtXSessionData<'a>),
    ExtXSessionKey(tags::ExtXSessionKey<'a>),
    ExtXIndependentSegments(tags::ExtXIndependentSegments),
    ExtXStart(tags::ExtXStart),
    VariantStream(tags::VariantStream<'a>),
    Unknown(&'a str),
}

// TODO: fold into crate::Error
#[derive(Debug)]
pub enum ParseError<'a> {
    Incomplete(&'static str),
    Unexpected { expected: &'a [u8], found: &'a [u8] },
    Utf8(&'a [u8]),
    InvalidNumber,
    ExpectedEndOfInput(&'a [u8]),
    Attributes,
}
type Result<'a, T> = std::result::Result<(&'a [u8], T), ParseError<'a>>;

enum SegmentState {
    AwaitExtinf,
    InSegment,
}

// TODO: reconcile with crate::MediaSegmentBuilder
struct MySegmentBuilder<'a> {
    state: SegmentState,
    segments: StableVec<MediaSegment<'a>>,
    program_date_time: Option<ExtXProgramDateTime<'a>>,
    date_range: Option<ExtXDateRange<'a>>,
    ext_inf: Option<ExtInf<'a>>,
}
impl<'a> Default for MySegmentBuilder<'a> {
    fn default() -> Self {
        MySegmentBuilder {
            state: SegmentState::AwaitExtinf,
            segments: StableVec::default(),
            program_date_time: None,
            date_range: None,
            ext_inf: None,
        }
    }
}
impl<'a> MySegmentBuilder<'a> {
    fn uri(&mut self, uri: LazyStr<'a>) {
        let uri = match uri.try_str() {
            Ok(v) => v,
            Err(e) => {
                // TODO: propagate Err somewhere
                println!("TODO: Bad UTF-8 in URL");
                return;
            },
        };
        match self.state {
            SegmentState::AwaitExtinf => {
                // TODO: propagate Err somewhere
                println!("Got uri without EXTINF tag, {:?}", uri);
            },
            SegmentState::InSegment => {
                let mut seg = MediaSegment::new(uri);
                seg.program_date_time = self.program_date_time.take();
                seg.duration = self.ext_inf.take().unwrap();
                self.segments.push(seg);
            },
        }
        self.state = SegmentState::AwaitExtinf;
    }
    fn date_range(&mut self, date_range: ExtXDateRange<'a>) {
        self.date_range = Some(date_range);
    }

    fn program_date_time(&mut self, date_time: ExtXProgramDateTime<'a>) {
        self.program_date_time = Some(date_time);
    }

    fn ext_inf(&mut self, duration: Duration, title: Option<LazyStr<'a>>) {
        self.ext_inf = Some(ExtInf::new(duration));
        self.state = SegmentState::InSegment;
    }
}

// TODO: reconcile with crate::MediaPlaylistBuilder
pub struct MyPlaylistBuilder<'a> {
    version: Option<ProtocolVersion>,
    media_sequence: Option<usize>,
    target_duration: Option<Duration>,
    independent_segments: bool,
    segment_builder: MySegmentBuilder<'a>,
}
impl<'a> Default for MyPlaylistBuilder<'a> {
    fn default() -> Self {
        MyPlaylistBuilder {
            version: None,
            media_sequence: None,
            target_duration: None,
            independent_segments: false,
            segment_builder: Default::default(),
        }
    }
}
impl<'a> MyPlaylistBuilder<'a> {
    fn add_line(&mut self, line: Line<'a>) {
        match line {
            Line::Tag(tag) => match tag {
                Tag::ExtXVersion(v) => {
                    if let Some(old_v) = self.version {
                        println!("EXT-X-VERSION redefined");
                    }
                    self.version = Some(v.version())
                },
                Tag::ExtInf { duration, title } => {
                    self.segment_builder.ext_inf(duration, title);
                },
                Tag::ExtXMediaSequence(seq) => {
                    self.media_sequence = Some(seq.0);
                }
                Tag::ExtXIndependentSegments(_) => {
                    self.independent_segments = true;
                }
                Tag::ExtXTargetDuration(d) => {
                    if let Some(old_v) = self.target_duration {
                        println!("EXT-X-TARGETDURATION redefined");
                    }
                    self.target_duration = Some(d.0)
                }
                Tag::ExtXProgramDateTime(datetime) => {
                    self.segment_builder.program_date_time(datetime);
                }
                Tag::ExtXDateRange(daterange) => {
                    self.segment_builder.date_range(daterange);
                }
                _ => unimplemented!("tag {:?}", tag)
            },
            Line::Comment(c) => {
                //println!("Comment: {:?}", c);
            },
            Line::Uri(uri) => {
                self.segment_builder.uri(uri);
            },
        }
    }

    pub fn build(self) -> std::result::Result<MediaPlaylist<'a>, Error> {
        Ok(MediaPlaylist {
            target_duration: self.target_duration.ok_or_else(|| Error::missing_tag("EXT-X-TARGETDURATION", ""))?,
            media_sequence: 0,
            discontinuity_sequence: 0,
            playlist_type: None,
            has_i_frames_only: false,
            has_independent_segments: false,
            start: None,
            has_end_list: false,
            segments: self.segment_builder.segments,
            allowable_excess_duration: Default::default(),
            unknown: vec![]
        })
    }
}

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
    pub fn take(&mut self, n: usize) -> std::result::Result<&'a[u8], ParseError<'a>> {
        if self.buf.len() < n {
            return Err(ParseError::Incomplete("take"))  // FIXME: this param to Incomplete makes no sense
        }
        self.pos += n;
        let (head, tail) = self.buf.split_at(n);
        self.buf = tail;
        Ok(head)
    }

    pub fn tag(&mut self, expected: &'static [u8]) -> std::result::Result<(), ParseError<'a>> {
        if self.buf.starts_with(expected) {
            self.pos += expected.len();
            self.buf = &self.buf[expected.len()..];
            Ok(())
        } else {
            Err(ParseError::Unexpected { expected, found: self.buf })
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn into_buf(self) -> &'a [u8] {
        self.buf
    }

    fn take_till_eol(&mut self) -> &'a[u8] {
        if let Some(pos) = memchr(b'\n', self.buf) {
            let (head, tail) = self.buf.split_at(pos);
            self.buf = tail;
            self.pos += pos;
            head
        } else {
            let res = self.buf.clone();
            self.pos += self.buf.len();
            self.buf = &self.buf[0..0];
            res
        }
    }

    fn u64(&mut self) -> std::result::Result<u64, ParseError<'a>> {
        match ::lexical_core::parse_partial(self.buf) {
            Ok((value, processed)) => {
                self.take(processed);
                Ok(value)
            },
            Err(_) => Err(ParseError::InvalidNumber)
        }
    }

    fn f64(&mut self) -> std::result::Result<f64, ParseError<'a>> {
        match ::lexical_core::parse_partial(self.buf) {
            Ok((value, processed)) => {
                self.take(processed);
                Ok(value)
            },
            Err(_) => Err(ParseError::InvalidNumber)
        }
    }
}


pub struct Parser<'a> {
    cursor: Cursor<'a>
}
impl<'a> Parser<'a> {
    pub fn new(cursor: Cursor<'a>) -> Parser<'a> {
        Parser {
            cursor
        }
    }
    pub fn parse(&mut self) -> std::result::Result<MyPlaylistBuilder<'a>, ParseError<'a>> {
        self.cursor.tag(b"#EXTM3U")?;
        self.line_ending()?;
        let builder = self.lines()?;
        if !self.cursor.is_empty() {
            return Err(ParseError::ExpectedEndOfInput(self.cursor.buf))
        }
        Ok(builder)
    }
    fn lines(&mut self) -> std::result::Result<MyPlaylistBuilder<'a>, ParseError<'a>> {
        let mut builder = MyPlaylistBuilder::default();
        loop {
            if self.cursor.is_empty() {
                return Ok(builder);
            }
            let line = self.hls_line()?;
            builder.add_line(line);
        }
    }

    fn hls_line(&mut self) -> std::result::Result<Line<'a>, ParseError<'a>> {
        let res = self.comment_or_tag();
        if res.is_ok() { return res; }

        let res = self.uri_line();
        if res.is_ok() { return res; }

        self.line_ending()?;
        Ok(Line::Comment(LazyStr(b"")))  // TODO: blank line
    }

    fn comment_or_tag(&mut self) -> std::result::Result<Line<'a>, ParseError<'a>> {
        self.cursor.tag(b"#")?;

        let res = self.ext_tag_eol().map(|r| Line::Tag(r));
        if res.is_ok() { return res; }

        self.comment()
    }

    fn ext_tag_eol(&mut self) -> std::result::Result<Tag<'a>, ParseError<'a>> {
        let res = self.ext_tag()?;

        self.line_ending();
        Ok(res)
    }
    fn ext_tag(&mut self) -> std::result::Result<Tag<'a>, ParseError<'a>> {
        self.cursor.tag(b"EXT")?;

        let res = self.ext_inf().map(|(duration, title)| Tag::ExtInf { duration, title });
        if res.is_ok() { return res; }
        let res = self.ext_date_range().map(Tag::ExtXDateRange);
        if res.is_ok() { return res; }
        let res = self.ext_program_date_time().map(Tag::ExtXProgramDateTime);
        if res.is_ok() { return res; }
        let res = self.ext_version().map(Tag::ExtXVersion);
        if res.is_ok() { return res; }
        let res = self.ext_media_sequence().map(Tag::ExtXMediaSequence);
        if res.is_ok() { return res; }
        let res = self.ext_independent_segments().map(Tag::ExtXIndependentSegments);
        if res.is_ok() { return res; }
        self.ext_target_duration().map(Tag::ExtXTargetDuration)
    }

    fn ext_version(&mut self) -> std::result::Result<ExtXVersion, ParseError<'a>> {
        self.cursor.tag(b"-X-VERSION:")?;
        let val = self.cursor.take(1)?;
        Ok(ExtXVersion::new(ProtocolVersion::from_str(utf8(val)?).map_err(|_| ParseError::Attributes)?))
    }

    fn ext_media_sequence(&mut self) -> std::result::Result<ExtXMediaSequence, ParseError<'a>> {
        self.cursor.tag(b"-X-MEDIA-SEQUENCE:")?;
        let msn = self.cursor.f64()?;
        Ok(ExtXMediaSequence(msn as usize))
    }

    fn ext_independent_segments(&mut self) -> std::result::Result<ExtXIndependentSegments, ParseError<'a>> {
        self.cursor.tag(b"-X-INDEPENDENT-SEGMENTS")?;
        Ok(ExtXIndependentSegments)
    }

    fn ext_target_duration(&mut self) -> std::result::Result<ExtXTargetDuration, ParseError<'a>> {
        self.cursor.tag(b"-X-TARGETDURATION:")?;
        let val = self.cursor.u64()?;
        Ok(ExtXTargetDuration(std::time::Duration::from_secs(val)))
    }


    fn ext_program_date_time(&mut self) -> std::result::Result<ExtXProgramDateTime<'a>, ParseError<'a>> {
        self.cursor.tag(b"-X-PROGRAM-DATE-TIME:")?;
        let val = self.cursor.take_till_eol();
        Ok(utf8(val).map(ExtXProgramDateTime::new)?)
    }

    fn ext_date_range(&mut self) -> std::result::Result<ExtXDateRange<'a>, ParseError<'a>> {
        self.cursor.tag(b"-X-DATERANGE:")?;
        let val = self.cursor.take_till_eol();
        utf8(val).and_then(|s| ExtXDateRange::from_str_attrs(s).map_err(|_| ParseError::Attributes))
    }

    fn comment(&mut self) -> std::result::Result<Line<'a>, ParseError<'a>> {
        let res = self.cursor.take_till_eol();
        let res = Line::Comment(LazyStr(res));

        self.line_ending();
        Ok(res)
    }

    fn ext_inf(&mut self) -> std::result::Result<(Duration, Option<LazyStr<'a>>), ParseError<'a>> {
        self.cursor.tag(b"INF:")?;
        let duration = self.duration()?;
        self.cursor.tag(b",")?;
        let description = self.description()?;

        //inf.set_title_string(description);
        Ok((duration, description))
    }

    fn uri_line(&mut self) -> std::result::Result<Line<'a>, ParseError<'a>> {
        if self.cursor.is_empty() {
            return Err(ParseError::Incomplete("uri"))
        }
        let res = self.cursor.take_till_eol();
        if res.is_empty() {
            return Err(ParseError::Incomplete("uri"))
        }
        let res = if res.ends_with(b"\r") {
            &res[..res.len() - 1]
        } else {
            res
        };
        let line = Line::Uri(LazyStr(res));
        self.line_ending();
        Ok(line)
    }


    fn duration(&mut self) -> std::result::Result<Duration, ParseError<'a>> {
        self.cursor.f64()
            .map(Duration::from_secs_f64)
    }
    fn description(&mut self) -> std::result::Result<Option<LazyStr<'a>>, ParseError<'a>> {
        let r = self.cursor.take_till_eol();
        if r.is_empty() {
            Ok(None)
        } else {
            Ok(Some(LazyStr(r)))
        }
    }

    fn line_ending(&mut self) -> std::result::Result<(), ParseError<'a>> {
        if self.cursor.is_empty() {
            Err(ParseError::Incomplete("line-end"))
        } else {
            if let Ok(_) = self.cursor.tag(b"\r\n") {
                Ok(())
            } else {
                self.cursor.tag(b"\n")
            }
        }
    }
}
fn utf8(input: &[u8]) -> std::result::Result<&str, ParseError<'_>> {
    std::str::from_utf8(input)
        .map_err(|_| ParseError::Utf8(input))
}

