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
pub struct MarginCommodity {
    #[serde(rename = "$ref")]
    margin_commodity_ref: String,

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

    #[serde(rename = "MarginCommodity")]
    margin_commodity: MarginCommodityClass,
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
    #[serde(rename = "available")]
    available: AvailableClass,

    #[serde(rename = "enabled")]
    enabled: AdhocMargin,

    #[serde(rename = "net")]
    net: AdhocMargin,

    #[serde(rename = "utilised")]
    utilised: Utilised,
}

#[derive(Serialize, Deserialize)]
pub struct AvailableClass {
    #[serde(rename = "$ref")]
    available_class_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct Utilised {
    #[serde(rename = "additionalProperties")]
    additional_properties: AdhocMargin,

    #[serde(rename = "type")]
    utilised_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MarginCommodityClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MarginCommodityProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    margin_commodity_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MarginCommodityProperties {
    #[serde(rename = "data")]
    data: AvailableClass,

    #[serde(rename = "status")]
    status: AdhocMargin,
}
