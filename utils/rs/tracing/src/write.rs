use std::{
    io::{self, Write},
    sync::Mutex,
    time::SystemTime,
};

use serde::{Serialize, Serializer};
use tracing::{
    field::{FieldSet, ValueSet, Visit},
    span::{self, Attributes, Id, Record},
    Event, Level, Metadata, Subscriber,
};
use tracing_subscriber::{layer::Context, registry::LookupSpan, Layer};

#[derive(Serialize)]
#[serde(transparent)]
struct SpanId(u64);
impl From<Id> for SpanId {
    fn from(value: Id) -> Self {
        Self(value.into_u64())
    }
}

#[derive(Serialize)]
struct Timestamp {
    secs: u64,
    nanos: u32,
}
impl Timestamp {
    fn now() -> Self {
        let t = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap();
        Self {
            secs: t.as_secs(),
            nanos: t.subsec_nanos(),
        }
    }
}

#[derive(Serialize)]
struct ErrorSource {
    debug: String,
    description: String,
}

#[derive(Serialize)]
struct Error {
    debug: String,
    description: String,
    sources: Vec<ErrorSource>,
}
impl Error {
    fn new(mut err: &dyn std::error::Error) -> Self {
        let mut sources = Vec::new();

        let debug = format!("{err:?}");
        let description = err.to_string();

        while let Some(e) = err.source() {
            sources.push(ErrorSource {
                debug: format!("{e:?}"),
                description: e.to_string(),
            });
            err = e;
        }
        Self {
            debug,
            description,
            sources,
        }
    }
}

struct ValueVisitor<S, E>(S, Option<E>);
impl<S: serde::ser::SerializeStruct> ValueVisitor<S, S::Error> {
    fn add_field<D: serde::Serialize + ?Sized>(
        &mut self,
        field: &tracing::field::Field,
        value: &D,
    ) {
        if self.1.is_none() {
            if let Err(e) = self.0.serialize_field(field.name(), value) {
                self.1 = Some(e)
            }
        }
    }
    fn finish(self) -> Result<S::Ok, S::Error> {
        match self.1 {
            Some(e) => Err(e),
            None => serde::ser::SerializeStruct::end(self.0),
        }
    }
}
impl<S: serde::ser::SerializeStruct> Visit for ValueVisitor<S, S::Error> {
    fn record_bool(&mut self, field: &tracing::field::Field, value: bool) {
        self.add_field(field, &value)
    }
    fn record_i64(&mut self, field: &tracing::field::Field, value: i64) {
        self.add_field(field, &value)
    }
    fn record_u64(&mut self, field: &tracing::field::Field, value: u64) {
        self.add_field(field, &value)
    }
    fn record_i128(&mut self, field: &tracing::field::Field, value: i128) {
        self.add_field(field, &value)
    }
    fn record_u128(&mut self, field: &tracing::field::Field, value: u128) {
        self.add_field(field, &value)
    }
    fn record_f64(&mut self, field: &tracing::field::Field, value: f64) {
        self.add_field(field, &value)
    }
    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        self.add_field(field, value)
    }
    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        self.add_field(field, &format!("{value:?}"))
    }
    fn record_error(
        &mut self,
        field: &tracing::field::Field,
        value: &(dyn std::error::Error + 'static),
    ) {
        self.add_field(field, &Error::new(value))
    }
}

fn serialize_value_set<S: Serializer>(val: &&ValueSet, serializer: S) -> Result<S::Ok, S::Error> {
    let mut ser = ValueVisitor(serializer.serialize_struct("ValueSet", val.len())?, None);
    val.record(&mut ser);
    ser.finish()
}

fn serialize_fields<S: Serializer>(val: &&FieldSet, serializer: S) -> Result<S::Ok, S::Error> {
    use serde::ser::SerializeSeq;
    let mut ser = serializer.serialize_seq(Some(val.len()))?;
    for f in val.iter() {
        ser.serialize_element(f.name())?;
    }
    ser.end()
}

fn serialize_level<S: Serializer>(val: &Level, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(val.as_str())
}

#[derive(Serialize)]
#[serde(rename = "Metadata")]
struct SerMetadata<'a> {
    name: &'static str,
    target: &'a str,
    #[serde(serialize_with = "serialize_level")]
    level: Level,
    module_path: Option<&'a str>,
    file: Option<&'a str>,
    line: Option<u32>,
    #[serde(serialize_with = "serialize_fields")]
    fields: &'a FieldSet,
}
impl<'a> SerMetadata<'a> {
    fn new(meta: &'a Metadata<'a>) -> Self {
        Self {
            name: meta.name(),
            target: meta.target(),
            level: *meta.level(),
            module_path: meta.module_path(),
            file: meta.file(),
            line: meta.line(),
            fields: meta.fields(),
        }
    }
}

#[derive(Serialize)]
#[serde(rename = "Attributes")]
struct SerAttrib<'a> {
    metadata: SerMetadata<'a>,
    #[serde(serialize_with = "serialize_value_set")]
    values: &'a ValueSet<'a>,
    parent: Option<SpanId>,
}
impl<'a> SerAttrib<'a> {
    fn new<S: Subscriber>(attr: &'a Attributes<'a>, context: Context<'_, S>) -> Self {
        Self {
            metadata: SerMetadata::new(attr.metadata()),
            values: attr.values(),
            parent: match attr.parent() {
                Some(p) => Some(p.clone().into()),
                None if attr.is_contextual() => {
                    context.current_span().id().cloned().map(SpanId::from)
                }
                None => None,
            },
        }
    }
}

struct SerEventFields<'a>(&'a Event<'a>);
impl<'a> Serialize for SerEventFields<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut ser = ValueVisitor(
            serializer.serialize_struct("ValueSet", self.0.metadata().fields().len())?,
            None,
        );
        self.0.record(&mut ser);
        ser.finish()
    }
}

#[derive(Serialize)]
#[serde(rename = "Event")]
struct SerEvent<'a> {
    metadata: SerMetadata<'a>,
    fields: SerEventFields<'a>,
    parent: Option<SpanId>,
}
impl<'a> SerEvent<'a> {
    fn new<S: Subscriber>(event: &'a Event<'a>, context: Context<'_, S>) -> Self {
        Self {
            metadata: SerMetadata::new(event.metadata()),
            fields: SerEventFields(event),
            parent: match event.parent() {
                Some(i) => Some(SpanId::from(i.clone())),
                None if event.is_contextual() => {
                    context.current_span().id().cloned().map(SpanId::from)
                }
                None => None,
            },
        }
    }
}

fn serialize_record<'a, S: Serializer>(
    val: &&Record<'a>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let mut ser = ValueVisitor(serializer.serialize_struct("ValueSet", val.len())?, None);
    val.record(&mut ser);
    ser.finish()
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
enum Message<'a> {
    NewSpan {
        span: SpanId,
        attrs: SerAttrib<'a>,
    },
    Record {
        span: SpanId,
        #[serde(serialize_with = "serialize_record")]
        values: &'a Record<'a>,
    },
    FollowsFrom {
        span: SpanId,
        follows: SpanId,
    },
    Event(SerEvent<'a>),
    Enter(SpanId),
    Exit(SpanId),
    Close(SpanId),
    IdChange {
        old: SpanId,
        new: SpanId,
    },
}
#[derive(Serialize)]
struct Entry<'a> {
    timestamp: Timestamp,
    message: Message<'a>,
}

#[derive(Default, Clone, Copy)]
struct IsRoot(bool);
impl Visit for IsRoot {
    fn record_bool(&mut self, field: &tracing::field::Field, value: bool) {
        if field.name() == "webar.root" {
            self.0 = value;
        }
    }
    fn record_debug(&mut self, _: &tracing::field::Field, _: &dyn std::fmt::Debug) {}
    fn record_u64(&mut self, _: &tracing::field::Field, _: u64) {}
    fn record_i64(&mut self, _: &tracing::field::Field, _: i64) {}
    fn record_i128(&mut self, _: &tracing::field::Field, _: i128) {}
    fn record_u128(&mut self, _: &tracing::field::Field, _: u128) {}
    fn record_f64(&mut self, _: &tracing::field::Field, _: f64) {}
    fn record_str(&mut self, _: &tracing::field::Field, _: &str) {}
    fn record_error(&mut self, _: &tracing::field::Field, _: &(dyn std::error::Error + 'static)) {}
}

struct Inner<W>(W);

pub struct WriteLayer<W>(Mutex<Option<Inner<W>>>);
impl<W: Write> WriteLayer<W> {
    pub fn new(mut writer: W) -> Result<Self, io::Error> {
        writer.write_all(&[0x9f])?; // start streaming array
        Ok(Self(Mutex::new(Some(Inner(writer)))))
    }
    fn add_entry(&self, entry: &Entry) {
        let mut cbor = Vec::new();
        ciborium::into_writer(entry, &mut cbor).unwrap();

        let mut json = serde_json::to_string(entry).unwrap();
        json.push('\n');

        let mut l = self.0.lock().unwrap();
        let w = l.as_mut().expect("write to finished log");
        w.0.write_all(&cbor).unwrap();
    }
}
impl<W: std::io::Write + 'static, S> Layer<S> for WriteLayer<W>
where
    S: Subscriber + for<'l> LookupSpan<'l>,
{
    fn on_new_span(&self, attrs: &span::Attributes<'_>, id: &span::Id, ctx: Context<'_, S>) {
        let mut is_root = IsRoot(false);
        attrs.record(&mut is_root);
        ctx.span(id).unwrap().extensions_mut().insert(is_root);

        self.add_entry(&Entry {
            timestamp: Timestamp::now(),
            message: Message::NewSpan {
                span: id.clone().into(),
                attrs: SerAttrib::new(attrs, ctx),
            },
        });
    }
    fn on_record(&self, span: &span::Id, values: &span::Record<'_>, _: Context<'_, S>) {
        self.add_entry(&Entry {
            timestamp: Timestamp::now(),
            message: Message::Record {
                span: span.clone().into(),
                values,
            },
        })
    }
    fn on_follows_from(&self, span: &span::Id, follows: &span::Id, _: Context<'_, S>) {
        self.add_entry(&Entry {
            timestamp: Timestamp::now(),
            message: Message::FollowsFrom {
                span: span.clone().into(),
                follows: follows.clone().into(),
            },
        })
    }
    fn on_event(&self, event: &Event<'_>, ctx: Context<'_, S>) {
        self.add_entry(&Entry {
            timestamp: Timestamp::now(),
            message: Message::Event(SerEvent::new(event, ctx)),
        })
    }
    fn on_enter(&self, id: &span::Id, _: Context<'_, S>) {
        self.add_entry(&Entry {
            timestamp: Timestamp::now(),
            message: Message::Enter(id.clone().into()),
        })
    }
    fn on_exit(&self, id: &span::Id, _: Context<'_, S>) {
        self.add_entry(&Entry {
            timestamp: Timestamp::now(),
            message: Message::Exit(id.clone().into()),
        })
    }
    fn on_close(&self, id: span::Id, ctx: Context<'_, S>) {
        self.add_entry(&Entry {
            timestamp: Timestamp::now(),
            message: Message::Close(id.clone().into()),
        });
        if let Some(sp) = ctx.span(&id) {
            if sp
                .extensions()
                .get::<IsRoot>()
                .copied()
                .unwrap_or_default()
                .0
            {
                let mut w = self.0.lock().unwrap().take().expect("finish log again");
                w.0.write_all(&[0xff]).unwrap(); // finish array
            }
        }
    }
    fn on_id_change(&self, old: &span::Id, new: &span::Id, _: Context<'_, S>) {
        self.add_entry(&Entry {
            timestamp: Timestamp::now(),
            message: Message::IdChange {
                old: old.clone().into(),
                new: new.clone().into(),
            },
        })
    }
}
