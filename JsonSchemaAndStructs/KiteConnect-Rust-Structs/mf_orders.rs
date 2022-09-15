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
pub struct MfOrders {
    #[serde(rename = "$ref")]
    mf_orders_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Datum")]
    datum: Datum,

    #[serde(rename = "MFOrders")]
    mf_orders: MfOrdersClass,
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
    #[serde(rename = "amount")]
    amount: Amount,

    #[serde(rename = "average_price")]
    average_price: Amount,

    #[serde(rename = "exchange_order_id")]
    exchange_order_id: ExchangeOrderId,

    #[serde(rename = "exchange_timestamp")]
    exchange_timestamp: ExchangeOrderId,

    #[serde(rename = "folio")]
    folio: Amount,

    #[serde(rename = "fund")]
    fund: Amount,

    #[serde(rename = "last_price")]
    last_price: Amount,

    #[serde(rename = "last_price_date")]
    last_price_date: LastPriceDate,

    #[serde(rename = "order_id")]
    order_id: LastPriceDate,

    #[serde(rename = "order_timestamp")]
    order_timestamp: LastPriceDate,

    #[serde(rename = "placed_by")]
    placed_by: Amount,

    #[serde(rename = "purchase_type")]
    purchase_type: Amount,

    #[serde(rename = "quantity")]
    quantity: Amount,

    #[serde(rename = "settlement_id")]
    settlement_id: ExchangeOrderId,

    #[serde(rename = "status")]
    status: Amount,

    #[serde(rename = "status_message")]
    status_message: Amount,

    #[serde(rename = "tag")]
    tag: Tag,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: Amount,

    #[serde(rename = "transaction_type")]
    transaction_type: Amount,

    #[serde(rename = "variety")]
    variety: Amount,
}

#[derive(Serialize, Deserialize)]
pub struct Amount {
    #[serde(rename = "type")]
    amount_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct ExchangeOrderId {
    #[serde(rename = "anyOf")]
    any_of: Vec<LastPriceDate>,
}

#[derive(Serialize, Deserialize)]
pub struct LastPriceDate {
    #[serde(rename = "format")]
    format: Option<String>,

    #[serde(rename = "type")]
    last_price_date_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct Tag {
    #[serde(rename = "anyOf")]
    any_of: Vec<Amount>,
}

#[derive(Serialize, Deserialize)]
pub struct MfOrdersClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MfOrdersProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    mf_orders_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfOrdersProperties {
    #[serde(rename = "data")]
    data: Data,

    #[serde(rename = "status")]
    status: Amount,
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
    #[serde(rename = "null")]
    Null,

    #[serde(rename = "number")]
    Number,

    #[serde(rename = "string")]
    String,
}
