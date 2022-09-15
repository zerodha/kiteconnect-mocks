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
pub struct OrderInfo {
    #[serde(rename = "$ref")]
    order_info_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Datum")]
    datum: Datum,

    #[serde(rename = "OrderInfo")]
    order_info: OrderInfoClass,
}

#[derive(Serialize, Deserialize)]
pub struct Datum {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: DatumProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    datum_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct DatumProperties {
    #[serde(rename = "average_price")]
    average_price: AveragePrice,

    #[serde(rename = "cancelled_quantity")]
    cancelled_quantity: AveragePrice,

    #[serde(rename = "disclosed_quantity")]
    disclosed_quantity: AveragePrice,

    #[serde(rename = "exchange")]
    exchange: AveragePrice,

    #[serde(rename = "exchange_order_id")]
    exchange_order_id: ExchangeOrderId,

    #[serde(rename = "exchange_timestamp")]
    exchange_timestamp: ExchangeTimestamp,

    #[serde(rename = "filled_quantity")]
    filled_quantity: AveragePrice,

    #[serde(rename = "instrument_token")]
    instrument_token: AveragePrice,

    #[serde(rename = "order_id")]
    order_id: AveragePrice,

    #[serde(rename = "order_timestamp")]
    order_timestamp: OrderTimestamp,

    #[serde(rename = "order_type")]
    order_type: AveragePrice,

    #[serde(rename = "parent_order_id")]
    parent_order_id: AveragePrice,

    #[serde(rename = "pending_quantity")]
    pending_quantity: AveragePrice,

    #[serde(rename = "placed_by")]
    placed_by: AveragePrice,

    #[serde(rename = "price")]
    price: AveragePrice,

    #[serde(rename = "product")]
    product: AveragePrice,

    #[serde(rename = "quantity")]
    quantity: AveragePrice,

    #[serde(rename = "status")]
    status: AveragePrice,

    #[serde(rename = "status_message")]
    status_message: AveragePrice,

    #[serde(rename = "tag")]
    tag: AveragePrice,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: AveragePrice,

    #[serde(rename = "transaction_type")]
    transaction_type: AveragePrice,

    #[serde(rename = "trigger_price")]
    trigger_price: AveragePrice,

    #[serde(rename = "validity")]
    validity: AveragePrice,

    #[serde(rename = "variety")]
    variety: AveragePrice,
}

#[derive(Serialize, Deserialize)]
pub struct AveragePrice {
    #[serde(rename = "type")]
    average_price_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct ExchangeOrderId {
    #[serde(rename = "anyOf")]
    any_of: Vec<AveragePrice>,
}

#[derive(Serialize, Deserialize)]
pub struct ExchangeTimestamp {
    #[serde(rename = "anyOf")]
    any_of: Vec<OrderTimestamp>,
}

#[derive(Serialize, Deserialize)]
pub struct OrderTimestamp {
    #[serde(rename = "format")]
    format: Option<String>,

    #[serde(rename = "type")]
    order_timestamp_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct OrderInfoClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: OrderInfoProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    order_info_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct OrderInfoProperties {
    #[serde(rename = "data")]
    data: Data,

    #[serde(rename = "status")]
    status: AveragePrice,
}

#[derive(Serialize, Deserialize)]
pub struct Data {
    #[serde(rename = "items")]
    items: Items,

    #[serde(rename = "type")]
    data_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct Items {
    #[serde(rename = "$ref")]
    items_ref: String,
}

#[derive(Serialize, Deserialize)]
pub enum Type {
    #[serde(rename = "integer")]
    Integer,

    #[serde(rename = "null")]
    Null,

    #[serde(rename = "number")]
    Number,

    #[serde(rename = "string")]
    String,
}
