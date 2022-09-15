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
pub struct SessionLogout {
    #[serde(rename = "$ref")]
    session_logout_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "SessionLogout")]
    session_logout: SessionLogoutClass,
}

#[derive(Serialize, Deserialize)]
pub struct SessionLogoutClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: Properties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    session_logout_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct Properties {
    #[serde(rename = "data")]
    data: Data,

    #[serde(rename = "status")]
    status: Data,
}

#[derive(Serialize, Deserialize)]
pub struct Data {
    #[serde(rename = "type")]
    data_type: String,
}
