// Example code that deserializes and serializes the model.
// extern crate serde;
// #[macro_use]
// extern crate serde_derive;
// extern crate serde_json;
//
// use generated_module::[object Object];
//
// fn main() {
//     let json = r#"{"answer": 42}"#;
//     let model: [object Object] = serde_json::from_str(&json).unwrap();
// }

extern crate serde_derive;

#[derive(Serialize, Deserialize)]
pub struct HistoricalMinute {
    #[serde(rename = "$ref")]
    historical_minute_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Candle")]
    candle: Candle,

    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "HistoricalMinute")]
    historical_minute: HistoricalMinuteClass,
}

#[derive(Serialize, Deserialize)]
pub struct Candle {
    #[serde(rename = "anyOf")]
    any_of: Vec<AnyOf>,

    #[serde(rename = "title")]
    title: String,
}

#[derive(Serialize, Deserialize)]
pub struct AnyOf {
    #[serde(rename = "type")]
    any_of_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct Data {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: DataProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    data_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct DataProperties {
    #[serde(rename = "candles")]
    candles: Candles,
}

#[derive(Serialize, Deserialize)]
pub struct Candles {
    #[serde(rename = "items")]
    items: Items,

    #[serde(rename = "type")]
    candles_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct Items {
    #[serde(rename = "items")]
    items: DataClass,

    #[serde(rename = "type")]
    items_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct DataClass {
    #[serde(rename = "$ref")]
    data_class_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct HistoricalMinuteClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: HistoricalMinuteProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    historical_minute_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct HistoricalMinuteProperties {
    #[serde(rename = "data")]
    data: DataClass,

    #[serde(rename = "status")]
    status: AnyOf,
}
