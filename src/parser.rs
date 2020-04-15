use crate::{MediaPlaylist, Error, MediaSegment, tags};
use nom::sequence::tuple;
use nom::branch::alt;
use nom::bytes::complete::{tag, is_a, take_while1, take_till, take_till1, take_until};
use nom::combinator::{not, map, map_res, recognize, opt, peek};
use nom::{IResult, CompareResult, InputLength};
use nom::Compare;
use nom::Slice;
use crate::tags::{ExtInf, ExtXVersion, ExtXMediaSequence, ExtXIndependentSegments, ExtXTargetDuration, ExtXProgramDateTime, ExtXDateRange};
use std::time::Duration;
use crate::types::{ProtocolVersion, PlaylistType};
use nom::error::{ErrorKind, ParseError};
use std::convert::TryInto;
use nom::bytes::streaming::take_while;
use smallvec::alloc::borrow::Cow;
use smallvec::alloc::string::FromUtf8Error;
use std::str::FromStr;
use nom::number::complete::double;
use std::ops::RangeFrom;
use nom::lib::std::convert::TryFrom;
use std::fmt;
use nom::lib::std::fmt::Formatter;
use memchr::memchr;

struct LazyStr<'a>(&'a[u8]);
impl<'a> LazyStr<'a> {
    fn try_to_string(&self) -> Result<String, std::string::FromUtf8Error> {
        String::from_utf8(Vec::from(self.0))
    }
}
impl<'a> fmt::Debug for LazyStr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

enum SegmentState {
    AraitExtinf,
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
            state: SegmentState::AraitExtinf,
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
            SegmentState::AraitExtinf => {
                println!("Got uri without EXTINF tag, {:?}", uri);
            },
            SegmentState::InSegment => {
                let mut seg = MediaSegment::new(uri);
                seg.program_date_time = self.program_date_time.take();
                seg.duration = self.ext_inf.take().unwrap();
                self.segments.push(seg);
            },
        }
        self.state = SegmentState::AraitExtinf;
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

struct MyPlaylistBuilder {
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

    fn build(self) -> Result<MediaPlaylist, Error> {
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
pub fn nom_parser(input: &[u8]) -> Result<MediaPlaylist, Error> {
    map(
        nom::combinator::all_consuming(
            tuple((
                nom::bytes::complete::tag(b"#EXTM3U"),
                nom::character::complete::line_ending,
                lines,
            ))
        ),
        |(_, _, p)| p,
    )
        (input)
        .map_err(|e| {
            match e {
                nom::Err::Incomplete(e) => {
                    println!("Incomplete {:?}", e);
                },
                nom::Err::Error((input, kind)) => {
                    println!("Error {:?}", kind);
                    println!("   {:?}", &String::from_utf8_lossy(input)[..100])
                },
                nom::Err::Failure(e) => {
                    println!("Failure {:?}", e);
                },
            }
            Error::invalid_input()
        } )
        .and_then(|(_, builder)| builder.build() )
}
fn lines(input: &[u8]) -> IResult<&[u8], MyPlaylistBuilder> {
    let mut builder = MyPlaylistBuilder::default();
    let mut iter = nom::combinator::iterator(input, hls_line);
    for line in &mut iter {
        builder.add_line(line);
    }
    iter.finish().map(|(i, _)| (i, builder) )
}

fn hls_line(input: &[u8]) -> IResult<&[u8], Line> {
    let (input, _) = take_till(|c| c!=b' ' && c!=b'\t' )(input)?;

    let res = comment_or_tag(input);
    if res.is_ok() { return res; }

    let res = uri_line(input);
    if res.is_ok() { return res; }

    map(peek(line_ending), |_| {println!("(blank)"); Line::Comment(LazyStr(b""))} )(input)  // TODO: blank line
}

fn comment_or_tag(input: &[u8]) -> IResult<&[u8], Line> {
    let (input, _) = tag(b"#")(input)?;

    let res = ext_tag_eol(input).map(|(i, r)| (i, Line::Tag(r)) );
    if res.is_ok() { return res; }

    comment(input)
}

fn ext_tag_eol(input: &[u8]) -> IResult<&[u8], Tag> {
    let (input, res) = ext_tag(input)?;

    if let Ok((trimmed_input, _)) = line_ending(input) {
        Ok((trimmed_input, res))
    } else {
        Ok((input, res))
    }
}
fn ext_tag(input: &[u8]) -> IResult<&[u8], Tag> {
    let (input, _) = tag(b"EXT")(input)?;

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

fn ext_version(input: &[u8]) -> IResult<&[u8], ExtXVersion> {
    map_res(
        tuple((
            tag(b"-X-VERSION:"),
            take_till(|c| c==b'\r' || c==b'\n' ),
        )),
        |(_, t)| std::str::from_utf8(t)
            .map_err(|_| nom::Err::Error((t, ErrorKind::Char)))
            .and_then(|s| s.parse().map_err(|_| nom::Err::Error((t, ErrorKind::Digit))) )
            .map(ExtXVersion::new)
    )
        (input)
}

fn ext_media_sequence(input: &[u8]) -> IResult<&[u8], ExtXMediaSequence> {
    map(
        tuple((
            tag(b"-X-MEDIA-SEQUENCE:"),
            parse_u64,
        )),
        |(_, t)| ExtXMediaSequence(t as usize)
    )
        (input)
}

fn ext_independent_segments(input: &[u8]) -> IResult<&[u8], ExtXIndependentSegments> {
    map(
        tag(b"-X-INDEPENDENT-SEGMENTS"),
        |_| ExtXIndependentSegments
    )
        (input)
}

fn ext_target_duration(input: &[u8]) -> IResult<&[u8], ExtXTargetDuration> {
    map(
        tuple((
            tag(b"-X-TARGETDURATION:"),
            parse_u64,
        )),
        |(_, t)| ExtXTargetDuration(std::time::Duration::from_secs(t))
    )
        (input)
}

fn ext_program_date_time(input: &[u8]) -> IResult<&[u8], ExtXProgramDateTime> {
    map_res(
        tuple((
            tag(b"-X-PROGRAM-DATE-TIME:"),
            take_till(|c| c==b'\r' || c==b'\n' ),
        )),
        |(_, t)| std::str::from_utf8(t)
            .map_err(|_| nom::Err::Error((t, ErrorKind::Char)))
            .map(ExtXProgramDateTime::new)
    )
        (input)
}

fn ext_date_range(input: &[u8]) -> IResult<&[u8], ExtXDateRange> {
    map_res(
        tuple((
            tag(b"-X-DATERANGE:"),
            take_till(|c| c==b'\r' || c==b'\n' ),
        )),
        |(_, c)| std::str::from_utf8(c)
            .map_err(|_| nom::Err::Error((c, ErrorKind::Char)))
            .and_then(|s| ExtXDateRange::from_str_attrs(s).map_err(|e| nom::Err::Error((c, ErrorKind::Verify)) ) )
    )
        (input)
}

fn comment(input: &[u8]) -> IResult<&[u8], Line> {
    let (input, res) = map(
        take_till(|c| c==b'\r' || c==b'\n' ),
        |c| Line::Comment(LazyStr(b""))  // TODO
    )
        (input)?;

    if let Ok((trimmed_input, _)) = line_ending(input) {
        Ok((trimmed_input, res))
    } else {
        Ok((input, res))
    }
}

fn ext_inf(input: &[u8]) -> IResult<&[u8], (Duration, Option<LazyStr<'_>>)> {
    let (input, _) = tag(b"INF:")(input)?;
    let (input, duration) = duration(input)?;
    let (input, _) = tag(b",")(input)?;
    let (input, description) = description(input)?;

    //inf.set_title_string(description);
    Ok((input, (duration, description)))
}

fn uri_line(input: &[u8]) -> IResult<&[u8], Line> {
    if input.is_empty() {
        return Err(nom::Err::Incomplete(nom::Needed::Size(1)))
    }
    let (res, input) = if let Some(pos) = memchr(b'\n', input) {
        if pos == 0 {
            return Err(nom::Err::Incomplete(nom::Needed::Size(1)))
        }
        let (a, b) = input.split_at(pos);
        (a, &b[1..])
    } else {
        (input, &input[0..0])
    };
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


fn duration(input: &[u8]) -> IResult<&[u8], Duration> {
    double(input)
        .map(|(i, r)| (i, Duration::from_secs_f64(r)) )
}
fn description(input: &[u8]) -> IResult<&[u8], Option<LazyStr>> {
    take_till(|c| c==b'\r' || c==b'\n' )(input)
        .and_then(|(i, r)| {
        if r.is_empty() {
            Ok((i, None))
        } else {
            Ok((i, Some(LazyStr(r))))
        }
    })
}


fn line_ending(input: &[u8]) -> IResult<&[u8], &[u8]> {
    match input.compare("\n") {
        CompareResult::Ok => Ok((input.slice(1..), input.slice(0..1))),
        CompareResult::Incomplete => IResult::Err(nom::Err::Error((input, ErrorKind::CrLf))),
        CompareResult::Error => {
            match input.compare("\r\n") {
                //FIXME: is this the right index?
                CompareResult::Ok => Ok((input.slice(2..), input.slice(0..2))),
                _ => IResult::Err(nom::Err::Error((input, ErrorKind::CrLf))),
            }
        }
    }
}

fn parse_u64<'a, E:ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], u64, E> {
    match ::lexical_core::parse_partial(input) {
        Ok((value, processed)) => Ok((input.slice(processed..), value)),
        Err(_) => Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)))
    }
}
