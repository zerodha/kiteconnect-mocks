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
pub struct MfOrderCancel {
    #[serde(rename = "$ref")]
    mf_order_cancel_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "MFOrderCancel")]
    mf_order_cancel: MfOrderCancelClass,
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
    #[serde(rename = "order_id")]
    order_id: OrderId,
}

#[derive(Serialize, Deserialize)]
pub struct OrderId {
    #[serde(rename = "format")]
    format: String,

    #[serde(rename = "type")]
    order_id_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfOrderCancelClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MfOrderCancelProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    mf_order_cancel_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfOrderCancelProperties {
    #[serde(rename = "data")]
    data: DataClass,

    #[serde(rename = "status")]
    status: Status,
}

#[derive(Serialize, Deserialize)]
pub struct DataClass {
    #[serde(rename = "$ref")]
    data_class_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct Status {
    #[serde(rename = "type")]
    status_type: String,
}
