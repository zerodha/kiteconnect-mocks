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
pub struct Margins {
    #[serde(rename = "$ref")]
    margins_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Available")]
    available: Available,

    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "Ity")]
    ity: Ity,

    #[serde(rename = "Margins")]
    margins: MarginsClass,
}

#[derive(Serialize, Deserialize)]
pub struct Available {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: AvailableProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    available_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct AvailableProperties {
    #[serde(rename = "adhoc_margin")]
    adhoc_margin: AdhocMargin,

    #[serde(rename = "cash")]
    cash: AdhocMargin,

    #[serde(rename = "collateral")]
    collateral: AdhocMargin,

    #[serde(rename = "intraday_payin")]
    intraday_payin: AdhocMargin,

    #[serde(rename = "live_balance")]
    live_balance: AdhocMargin,

    #[serde(rename = "opening_balance")]
    opening_balance: AdhocMargin,
}

#[derive(Serialize, Deserialize)]
pub struct AdhocMargin {
    #[serde(rename = "type")]
    adhoc_margin_type: String,
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
    #[serde(rename = "commodity")]
    commodity: Commodity,

    #[serde(rename = "equity")]
    equity: Commodity,
}

#[derive(Serialize, Deserialize)]
pub struct Commodity {
    #[serde(rename = "$ref")]
    commodity_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct Ity {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: ItyProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    ity_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct ItyProperties {
    #[serde(rename = "available")]
    available: Commodity,

    #[serde(rename = "enabled")]
    enabled: AdhocMargin,

    #[serde(rename = "net")]
    net: AdhocMargin,

    #[serde(rename = "utilised")]
    utilised: Utilised,
}

#[derive(Serialize, Deserialize)]
pub struct Utilised {
    #[serde(rename = "additionalProperties")]
    additional_properties: AdhocMargin,

    #[serde(rename = "type")]
    utilised_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MarginsClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MarginsProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    margins_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MarginsProperties {
    #[serde(rename = "data")]
    data: Commodity,

    #[serde(rename = "status")]
    status: AdhocMargin,
}
