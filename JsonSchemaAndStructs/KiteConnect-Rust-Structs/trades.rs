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
pub struct Trades {
    #[serde(rename = "$ref")]
    trades_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Datum")]
    datum: Datum,

    #[serde(rename = "Trades")]
    trades: TradesClass,
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

    #[serde(rename = "exchange")]
    exchange: AveragePrice,

    #[serde(rename = "exchange_order_id")]
    exchange_order_id: AveragePrice,

    #[serde(rename = "exchange_timestamp")]
    exchange_timestamp: ExchangeTimestamp,

    #[serde(rename = "fill_timestamp")]
    fill_timestamp: ExchangeTimestamp,

    #[serde(rename = "instrument_token")]
    instrument_token: AveragePrice,

    #[serde(rename = "order_id")]
    order_id: AveragePrice,

    #[serde(rename = "order_timestamp")]
    order_timestamp: ExchangeTimestamp,

    #[serde(rename = "product")]
    product: AveragePrice,

    #[serde(rename = "quantity")]
    quantity: AveragePrice,

    #[serde(rename = "trade_id")]
    trade_id: ExchangeTimestamp,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: AveragePrice,

    #[serde(rename = "transaction_type")]
    transaction_type: AveragePrice,
}

#[derive(Serialize, Deserialize)]
pub struct AveragePrice {
    #[serde(rename = "type")]
    average_price_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct ExchangeTimestamp {
    #[serde(rename = "format")]
    format: String,

    #[serde(rename = "type")]
    exchange_timestamp_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct TradesClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: TradesProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    trades_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct TradesProperties {
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

    #[serde(rename = "number")]
    Number,

    #[serde(rename = "string")]
    String,
}
