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
pub struct Profile {
    #[serde(rename = "$ref")]
    profile_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "Meta")]
    meta: MetaClass,

    #[serde(rename = "Profile")]
    profile: ProfileClass,
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
    #[serde(rename = "avatar_url")]
    avatar_url: AvatarUrl,

    #[serde(rename = "broker")]
    broker: AvatarUrl,

    #[serde(rename = "email")]
    email: AvatarUrl,

    #[serde(rename = "exchanges")]
    exchanges: Exchanges,

    #[serde(rename = "meta")]
    meta: Meta,

    #[serde(rename = "order_types")]
    order_types: Exchanges,

    #[serde(rename = "products")]
    products: Exchanges,

    #[serde(rename = "user_id")]
    user_id: AvatarUrl,

    #[serde(rename = "user_name")]
    user_name: AvatarUrl,

    #[serde(rename = "user_shortname")]
    user_shortname: AvatarUrl,

    #[serde(rename = "user_type")]
    user_type: AvatarUrl,
}

#[derive(Serialize, Deserialize)]
pub struct AvatarUrl {
    #[serde(rename = "type")]
    avatar_url_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct Exchanges {
    #[serde(rename = "items")]
    items: AvatarUrl,

    #[serde(rename = "type")]
    exchanges_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct Meta {
    #[serde(rename = "$ref")]
    meta_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct MetaClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MetaProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    meta_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MetaProperties {
    #[serde(rename = "demat_consent")]
    demat_consent: AvatarUrl,
}

#[derive(Serialize, Deserialize)]
pub struct ProfileClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: ProfileProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    profile_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct ProfileProperties {
    #[serde(rename = "data")]
    data: Meta,

    #[serde(rename = "status")]
    status: AvatarUrl,
}

#[derive(Serialize, Deserialize)]
pub enum Type {
    #[serde(rename = "null")]
    Null,

    #[serde(rename = "string")]
    String,
}
