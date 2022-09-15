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
pub struct TriggerRange {
    #[serde(rename = "$ref")]
    trigger_range_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "Nse")]
    nse: Nse,

    #[serde(rename = "TriggerRange")]
    trigger_range: TriggerRangeClass,
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
    #[serde(rename = "NSE:INFY")]
    nse_infy: NseInfy,

    #[serde(rename = "NSE:RELIANCE")]
    nse_reliance: NseInfy,
}

#[derive(Serialize, Deserialize)]
pub struct NseInfy {
    #[serde(rename = "$ref")]
    nse_infy_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct Nse {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: NseProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    nse_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct NseProperties {
    #[serde(rename = "instrument_token")]
    instrument_token: InstrumentToken,

    #[serde(rename = "lower")]
    lower: InstrumentToken,

    #[serde(rename = "upper")]
    upper: InstrumentToken,
}

#[derive(Serialize, Deserialize)]
pub struct InstrumentToken {
    #[serde(rename = "type")]
    instrument_token_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct TriggerRangeClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: TriggerRangeProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    trigger_range_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct TriggerRangeProperties {
    #[serde(rename = "data")]
    data: NseInfy,

    #[serde(rename = "status")]
    status: InstrumentToken,
}
