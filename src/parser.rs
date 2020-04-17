use crate::{MediaPlaylist, Error, MediaSegment, tags};
use crate::tags::{ExtInf, ExtXVersion, ExtXMediaSequence, ExtXIndependentSegments, ExtXTargetDuration, ExtXProgramDateTime, ExtXDateRange};
use std::time::Duration;
use crate::types::{ProtocolVersion, PlaylistType};
use std::convert::TryInto;
use smallvec::alloc::borrow::Cow;
use smallvec::alloc::string::FromUtf8Error;
use std::str::FromStr;
use std::ops::RangeFrom;
use std::fmt;
use memchr::memchr;
use std::io::BufRead;

struct LazyStr<'a>(&'a[u8]);
impl<'a> LazyStr<'a> {
    fn try_to_string(&self) -> std::result::Result<String, std::string::FromUtf8Error> {
        String::from_utf8(Vec::from(self.0))
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
    ExtXKey(tags::ExtXKey),
    ExtXMap(tags::ExtXMap),
    ExtXProgramDateTime(tags::ExtXProgramDateTime),
    ExtXDateRange(tags::ExtXDateRange),
    ExtXTargetDuration(tags::ExtXTargetDuration),
    ExtXMediaSequence(tags::ExtXMediaSequence),
    ExtXDiscontinuitySequence(tags::ExtXDiscontinuitySequence),
    ExtXEndList(tags::ExtXEndList),
    PlaylistType(PlaylistType),
    ExtXIFramesOnly(tags::ExtXIFramesOnly),
    ExtXMedia(tags::ExtXMedia),
    ExtXSessionData(tags::ExtXSessionData),
    ExtXSessionKey(tags::ExtXSessionKey),
    ExtXIndependentSegments(tags::ExtXIndependentSegments),
    ExtXStart(tags::ExtXStart),
    VariantStream(tags::VariantStream),
    Unknown(&'a str),
}

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

struct MySegmentBuilder {
    state: SegmentState,
    segments: Vec<MediaSegment>,
    program_date_time: Option<ExtXProgramDateTime>,
    date_range: Option<ExtXDateRange>,
    ext_inf: Option<ExtInf>,
}
impl Default for MySegmentBuilder {
    fn default() -> Self {
        MySegmentBuilder {
            state: SegmentState::AwaitExtinf,
            segments: vec![],
            program_date_time: None,
            date_range: None,
            ext_inf: None,
        }
    }
}
impl MySegmentBuilder {
    fn uri(&mut self, uri: LazyStr<'_>) {
        let uri = match uri.try_to_string() {
            Ok(v) => v,
            Err(e) => {
                println!("Bad UTF-8 in URL");
                return;
            },
        };
        match self.state {
            SegmentState::AwaitExtinf => {
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
    fn date_range(&mut self, date_range: ExtXDateRange) {
        self.date_range = Some(date_range);
    }

    fn program_date_time(&mut self, date_time: ExtXProgramDateTime) {
        self.program_date_time = Some(date_time);
    }

    fn ext_inf(&mut self, duration: Duration, title: Option<LazyStr<'_>>) {
        self.ext_inf = Some(ExtInf::new(duration));
        self.state = SegmentState::InSegment;
    }
}

pub struct MyPlaylistBuilder {
    version: Option<ProtocolVersion>,
    media_sequence: Option<usize>,
    target_duration: Option<Duration>,
    independent_segments: bool,
    segment_builder: MySegmentBuilder,
}
impl Default for MyPlaylistBuilder {
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
impl MyPlaylistBuilder {
    fn add_line(&mut self, line: Line) {
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

    pub fn build(self) -> std::result::Result<MediaPlaylist, Error> {
        Ok(MediaPlaylist {
            target_duration: self.target_duration.ok_or_else(|| Error::missing_tag("EXT-X-TARGETDURATION", ""))?,
            media_sequence: 0,
            discontinuity_sequence: 0,
            playlist_type: None,
            has_i_frames_only: false,
            has_independent_segments: false,
            start: None,
            has_end_list: false,
            segments: stable_vec::StableVec::default(),
            allowable_excess_duration: Default::default(),
            unknown: vec![]
        })
    }
}

fn tag<'a>(input: &'a[u8], val: &'static [u8]) -> Result<'a, ()> {
    if input.len() < val.len() {
        return Err(ParseError::Incomplete("tag"))
    }
    if input.starts_with(val) {
        Ok((&input[val.len()..], ()))
    } else {
        Err(ParseError::Unexpected { expected: val, found: &input[..val.len()] })
    }
}
pub fn new_parser(input: &[u8]) -> std::result::Result<MyPlaylistBuilder, ParseError> {
    let (input, _) = tag(input, b"#EXTM3U")?;
    let (input, _) = line_ending(input)?;
    let (input, builder) = lines(input)?;
    if !input.is_empty() {
        return Err(ParseError::ExpectedEndOfInput(input))
    }
    Ok(builder)
}
fn lines(mut input: &[u8]) -> Result<MyPlaylistBuilder> {
    let mut builder = MyPlaylistBuilder::default();
    loop {
        if input.is_empty() {
            return Ok((input, builder));
        }
        let (remain, line) = hls_line(input)?;
        builder.add_line(line);
        input = remain;
    }
}

fn hls_line(input: &[u8]) -> Result<Line> {
    let res = comment_or_tag(input);
    if res.is_ok() { return res; }

    let res = uri_line(input);
    if res.is_ok() { return res; }

    if let Ok((line,_)) = line_ending(input) {
        Ok((input, Line::Comment(LazyStr(b""))))  // TODO: blank line
    } else {
        res
    }
}

fn comment_or_tag(input: &[u8]) -> Result<Line> {
    let (input, _) = tag(input, b"#")?;

    let res = ext_tag_eol(input).map(|(i, r)| (i, Line::Tag(r)) );
    if res.is_ok() { return res; }

    comment(input)
}

fn ext_tag_eol(input: &[u8]) -> Result<Tag> {
    let (input, res) = ext_tag(input)?;

    if let Ok((trimmed_input, _)) = line_ending(input) {
        Ok((trimmed_input, res))
    } else {
        Ok((input, res))
    }
}
fn ext_tag(input: &[u8]) -> Result<Tag> {
    let (input, _) = tag(input, b"EXT")?;

    let res = ext_inf(input).map(|(input, (duration, title))| (input, Tag::ExtInf { duration, title }));
    if res.is_ok() { return res; }
    let res = ext_date_range(input).map(|(input, t)| (input, Tag::ExtXDateRange(t)));
    if res.is_ok() { return res; }
    let res = ext_program_date_time(input).map(|(input, t)| (input, Tag::ExtXProgramDateTime(t)));
    if res.is_ok() { return res; }
    let res = ext_version(input).map(|(input, t)| (input, Tag::ExtXVersion(t)));
    if res.is_ok() { return res; }
    let res = ext_media_sequence(input).map(|(input, t)| (input, Tag::ExtXMediaSequence(t)));
    if res.is_ok() { return res; }
    let res = ext_independent_segments(input).map(|(input, t)| (input, Tag::ExtXIndependentSegments(t)));
    if res.is_ok() { return res; }
    ext_target_duration(input).map(|(input, t)| (input, Tag::ExtXTargetDuration(t)))
}

fn slice(input: &[u8], size: usize) -> Result<&[u8]> {
    if size > input.len() {
        Err(ParseError::Incomplete("slice"))
    } else {
        Ok((&input[size..], &input[..size]))
    }
}

fn utf8(input: &[u8]) -> std::result::Result<&str, ParseError> {
    std::str::from_utf8(input)
        .map_err(|_| ParseError::Utf8(input))
}

fn ext_version(input: &[u8]) -> Result<ExtXVersion> {
    let (input, _) = tag(input, b"-X-VERSION:")?;
    let (input, val) = slice(input, 1)?;
    Ok((input, ExtXVersion::new(ProtocolVersion::from_str(utf8(val)?).map_err(|_| ParseError::Attributes)?)))
}

fn ext_media_sequence(input: &[u8]) -> Result<ExtXMediaSequence> {
    let (input, _) = tag(input, b"-X-MEDIA-SEQUENCE:")?;
    let (input, msn) = parse_u64(input)?;
    Ok((input, ExtXMediaSequence(msn as usize)))
}

fn ext_independent_segments(input: &[u8]) -> Result<ExtXIndependentSegments> {
    let (input, _) = tag(input, b"-X-INDEPENDENT-SEGMENTS")?;
    Ok((input, ExtXIndependentSegments))
}

fn ext_target_duration(input: &[u8]) -> Result<ExtXTargetDuration> {
    let (input, _) = tag(input, b"-X-TARGETDURATION:")?;
    let (input, val) = parse_u64(input)?;
    Ok((input, ExtXTargetDuration(std::time::Duration::from_secs(val))))
}

fn take_till_eol(input: &[u8]) -> (&[u8], &[u8]) {
    if let Some(pos) = memchr(b'\n', input) {
        let (res, input) = input.split_at(pos);
            (input, res)
    } else {
        (&input[0..0], input)
    }
}

fn ext_program_date_time(input: &[u8]) -> Result<ExtXProgramDateTime> {
    let (input, _) = tag(input, b"-X-PROGRAM-DATE-TIME:")?;
    let (input, val) = take_till_eol(input);
    Ok((input, utf8(val).map(ExtXProgramDateTime::new)?))
}

fn ext_date_range(input: &[u8]) -> Result<ExtXDateRange> {
    let (input, _) = tag(input, b"-X-DATERANGE:")?;
    let (input, val) = take_till_eol(input);
    let res = utf8(val).and_then(|s| ExtXDateRange::from_str_attrs(s).map_err(|_| ParseError::Attributes))?;
    Ok((input,  res))
}

fn comment(input: &[u8]) -> Result<Line> {
    let (input, res) = take_till_eol(input);
    let res = Line::Comment(LazyStr(res));

    if let Ok((trimmed_input, _)) = line_ending(input) {
        Ok((trimmed_input, res))
    } else {
        Ok((input, res))
    }
}

fn ext_inf(input: &[u8]) -> Result<(Duration, Option<LazyStr<'_>>)> {
    let (input, _) = tag(input, b"INF:")?;
    let (input, duration) = duration(input)?;
    let (input, _) = tag(input, b",")?;
    let (input, description) = description(input)?;

    //inf.set_title_string(description);
    Ok((input, (duration, description)))
}

fn uri_line(input: &[u8]) -> Result<Line> {
    let o = input.clone();
    if input.is_empty() {
        return Err(ParseError::Incomplete("uri"))
    }
    let (input, res) = take_till_eol(input);
    if res.is_empty() {
        return Err(ParseError::Incomplete("uri"))
    }
    let res = if res.ends_with(b"\r") {
        &res[..res.len()-1]
    } else {
        res
    };
    let line = Line::Uri(LazyStr(res));
    if let Ok((trimmed_input, _)) = line_ending(input) {
        Ok((trimmed_input, line))
    } else {
        Ok((input, line))
    }
}


fn duration(input: &[u8]) -> Result<Duration> {
    parse_f64(input)
        .map(|(i, r)| (i, Duration::from_secs_f64(r)) )
}
fn description(input: &[u8]) -> Result<Option<LazyStr>> {
    let (i, r) = take_till_eol(input);
    if r.is_empty() {
        Ok((i, None))
    } else {
        Ok((i, Some(LazyStr(r))))
    }
}


fn line_ending(input: &[u8]) -> Result<()> {
    if input.is_empty() {
        Err(ParseError::Incomplete("line-end"))
    } else {
        if input.len() > 2 {
            if let Ok((input, _)) = tag(input, b"\r\n") {
                Ok((input, ()))
            } else {
                tag(input, b"\n")
            }
        } else {
            tag(input, b"\n")
        }
    }
}

fn parse_u64(input: &[u8]) -> Result<u64> {
    match ::lexical_core::parse_partial(input) {
        Ok((value, processed)) => Ok((&input[processed..], value)),
        Err(_) => Err(ParseError::InvalidNumber)
    }
}
fn parse_f64(input: &[u8]) -> Result<f64> {
    match ::lexical_core::parse_partial(input) {
        Ok((value, processed)) => Ok((&input[processed..], value)),
        Err(_) => Err(ParseError::InvalidNumber)
    }
}
