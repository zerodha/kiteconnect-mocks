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
pub struct MfSipPlace {
    #[serde(rename = "$ref")]
    mf_sip_place_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "MFSIPPlace")]
    mfsip_place: MfsipPlaceClass,
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
    #[serde(rename = "sip_id")]
    sip_id: Sipid,
}

#[derive(Serialize, Deserialize)]
pub struct Sipid {
    #[serde(rename = "type")]
    sipid_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfsipPlaceClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MfsipPlaceProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    mfsip_place_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfsipPlaceProperties {
    #[serde(rename = "data")]
    data: DataClass,

    #[serde(rename = "status")]
    status: Sipid,
}

#[derive(Serialize, Deserialize)]
pub struct DataClass {
    #[serde(rename = "$ref")]
    data_class_ref: String,
}
