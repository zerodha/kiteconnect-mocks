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
pub struct Postback {
    #[serde(rename = "$ref")]
    postback_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Meta")]
    meta: Meta,

    #[serde(rename = "Postback")]
    postback: PostbackClass,
}

#[derive(Serialize, Deserialize)]
pub struct Meta {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    meta_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct PostbackClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: Properties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    postback_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct Properties {
    #[serde(rename = "app_id")]
    app_id: AppId,

    #[serde(rename = "average_price")]
    average_price: AppId,

    #[serde(rename = "cancelled_quantity")]
    cancelled_quantity: AppId,

    #[serde(rename = "checksum")]
    checksum: AppId,

    #[serde(rename = "disclosed_quantity")]
    disclosed_quantity: AppId,

    #[serde(rename = "exchange")]
    exchange: AppId,

    #[serde(rename = "exchange_order_id")]
    exchange_order_id: AppId,

    #[serde(rename = "exchange_timestamp")]
    exchange_timestamp: Timestamp,

    #[serde(rename = "exchange_update_timestamp")]
    exchange_update_timestamp: Timestamp,

    #[serde(rename = "filled_quantity")]
    filled_quantity: AppId,

    #[serde(rename = "guid")]
    guid: AppId,

    #[serde(rename = "instrument_token")]
    instrument_token: AppId,

    #[serde(rename = "market_protection")]
    market_protection: AppId,

    #[serde(rename = "meta")]
    meta: MetaClass,

    #[serde(rename = "order_id")]
    order_id: AppId,

    #[serde(rename = "order_timestamp")]
    order_timestamp: Timestamp,

    #[serde(rename = "order_type")]
    order_type: AppId,

    #[serde(rename = "parent_order_id")]
    parent_order_id: AppId,

    #[serde(rename = "pending_quantity")]
    pending_quantity: AppId,

    #[serde(rename = "placed_by")]
    placed_by: AppId,

    #[serde(rename = "price")]
    price: AppId,

    #[serde(rename = "product")]
    product: AppId,

    #[serde(rename = "quantity")]
    quantity: AppId,

    #[serde(rename = "status")]
    status: AppId,

    #[serde(rename = "status_message")]
    status_message: AppId,

    #[serde(rename = "status_message_raw")]
    status_message_raw: AppId,

    #[serde(rename = "tag")]
    tag: AppId,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: AppId,

    #[serde(rename = "transaction_type")]
    transaction_type: AppId,

    #[serde(rename = "trigger_price")]
    trigger_price: AppId,

    #[serde(rename = "unfilled_quantity")]
    unfilled_quantity: AppId,

    #[serde(rename = "user_id")]
    user_id: AppId,

    #[serde(rename = "validity")]
    validity: AppId,

    #[serde(rename = "variety")]
    variety: AppId,
}

#[derive(Serialize, Deserialize)]
pub struct AppId {
    #[serde(rename = "type")]
    app_id_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct Timestamp {
    #[serde(rename = "format")]
    format: String,

    #[serde(rename = "type")]
    timestamp_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct MetaClass {
    #[serde(rename = "$ref")]
    meta_class_ref: String,
}

#[derive(Serialize, Deserialize)]
pub enum Type {
    #[serde(rename = "integer")]
    Integer,

    #[serde(rename = "null")]
    Null,

    #[serde(rename = "string")]
    String,
}
