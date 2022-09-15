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
pub struct Orders {
    #[serde(rename = "$ref")]
    orders_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Datum")]
    datum: Datum,

    #[serde(rename = "Iceberg")]
    iceberg: Iceberg,

    #[serde(rename = "Meta")]
    meta: MetaClass,

    #[serde(rename = "Orders")]
    orders: OrdersClass,
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
    exchange_timestamp: ExchangeETimestamp,

    #[serde(rename = "exchange_update_timestamp")]
    exchange_update_timestamp: ExchangeETimestamp,

    #[serde(rename = "filled_quantity")]
    filled_quantity: AveragePrice,

    #[serde(rename = "guid")]
    guid: AveragePrice,

    #[serde(rename = "instrument_token")]
    instrument_token: AveragePrice,

    #[serde(rename = "market_protection")]
    market_protection: AveragePrice,

    #[serde(rename = "meta")]
    meta: Meta,

    #[serde(rename = "modified")]
    modified: AveragePrice,

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
    status_message: ExchangeOrderId,

    #[serde(rename = "status_message_raw")]
    status_message_raw: ExchangeOrderId,

    #[serde(rename = "tag")]
    tag: ExchangeOrderId,

    #[serde(rename = "tags")]
    tags: Tags,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: AveragePrice,

    #[serde(rename = "transaction_type")]
    transaction_type: AveragePrice,

    #[serde(rename = "trigger_price")]
    trigger_price: AveragePrice,

    #[serde(rename = "validity")]
    validity: AveragePrice,

    #[serde(rename = "validity_ttl")]
    validity_ttl: AveragePrice,

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
pub struct ExchangeETimestamp {
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
pub struct Meta {
    #[serde(rename = "$ref")]
    meta_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct Tags {
    #[serde(rename = "items")]
    items: AveragePrice,

    #[serde(rename = "type")]
    tags_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct Iceberg {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: IcebergProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    iceberg_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct IcebergProperties {
    #[serde(rename = "leg")]
    leg: AveragePrice,

    #[serde(rename = "leg_quantity")]
    leg_quantity: AveragePrice,

    #[serde(rename = "legs")]
    legs: AveragePrice,

    #[serde(rename = "remaining_quantity")]
    remaining_quantity: AveragePrice,

    #[serde(rename = "total_quantity")]
    total_quantity: AveragePrice,
}

#[derive(Serialize, Deserialize)]
pub struct MetaClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MetaProperties,

    #[serde(rename = "required")]
    required: Vec<Option<serde_json::Value>>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    meta_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MetaProperties {
    #[serde(rename = "iceberg")]
    iceberg: Meta,
}

#[derive(Serialize, Deserialize)]
pub struct OrdersClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: OrdersProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    orders_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct OrdersProperties {
    #[serde(rename = "data")]
    data: Data,

    #[serde(rename = "status")]
    status: AveragePrice,
}

#[derive(Serialize, Deserialize)]
pub struct Data {
    #[serde(rename = "items")]
    items: Meta,

    #[serde(rename = "type")]
    data_type: String,
}

#[derive(Serialize, Deserialize)]
pub enum Type {
    #[serde(rename = "boolean")]
    Boolean,

    #[serde(rename = "integer")]
    Integer,

    #[serde(rename = "null")]
    Null,

    #[serde(rename = "string")]
    String,
}
