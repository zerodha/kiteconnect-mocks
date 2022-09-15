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
pub struct Quote {
    #[serde(rename = "$ref")]
    quote_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Buy")]
    buy: Buy,

    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "Depth")]
    depth: Depth,

    #[serde(rename = "NseInfy")]
    nse_infy: NseInfyClass,

    #[serde(rename = "Ohlc")]
    ohlc: Ohlc,

    #[serde(rename = "Quote")]
    quote: QuoteClass,
}

#[derive(Serialize, Deserialize)]
pub struct Buy {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: BuyProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    buy_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct BuyProperties {
    #[serde(rename = "orders")]
    orders: Orders,

    #[serde(rename = "price")]
    price: Orders,

    #[serde(rename = "quantity")]
    quantity: Orders,
}

#[derive(Serialize, Deserialize)]
pub struct Orders {
    #[serde(rename = "type")]
    orders_type: Type,
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
    #[serde(rename = "NSE:INFY")]
    nse_infy: NseInfy,
}

#[derive(Serialize, Deserialize)]
pub struct NseInfy {
    #[serde(rename = "$ref")]
    nse_infy_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct Depth {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: DepthProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    depth_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct DepthProperties {
    #[serde(rename = "buy")]
    buy: BuyClass,

    #[serde(rename = "sell")]
    sell: BuyClass,
}

#[derive(Serialize, Deserialize)]
pub struct BuyClass {
    #[serde(rename = "items")]
    items: NseInfy,

    #[serde(rename = "type")]
    buy_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct NseInfyClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: NseInfyProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    nse_infy_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct NseInfyProperties {
    #[serde(rename = "average_price")]
    average_price: Orders,

    #[serde(rename = "buy_quantity")]
    buy_quantity: Orders,

    #[serde(rename = "depth")]
    depth: NseInfy,

    #[serde(rename = "instrument_token")]
    instrument_token: Orders,

    #[serde(rename = "last_price")]
    last_price: Orders,

    #[serde(rename = "last_quantity")]
    last_quantity: Orders,

    #[serde(rename = "last_trade_time")]
    last_trade_time: LastTradeTime,

    #[serde(rename = "lower_circuit_limit")]
    lower_circuit_limit: Orders,

    #[serde(rename = "net_change")]
    net_change: Orders,

    #[serde(rename = "ohlc")]
    ohlc: NseInfy,

    #[serde(rename = "oi")]
    oi: Orders,

    #[serde(rename = "oi_day_high")]
    oi_day_high: Orders,

    #[serde(rename = "oi_day_low")]
    oi_day_low: Orders,

    #[serde(rename = "sell_quantity")]
    sell_quantity: Orders,

    #[serde(rename = "timestamp")]
    timestamp: LastTradeTime,

    #[serde(rename = "upper_circuit_limit")]
    upper_circuit_limit: Orders,

    #[serde(rename = "volume")]
    volume: Orders,
}

#[derive(Serialize, Deserialize)]
pub struct LastTradeTime {
    #[serde(rename = "format")]
    format: String,

    #[serde(rename = "type")]
    last_trade_time_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct Ohlc {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: OhlcProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    ohlc_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct OhlcProperties {
    #[serde(rename = "close")]
    close: Orders,

    #[serde(rename = "high")]
    high: Orders,

    #[serde(rename = "low")]
    low: Orders,

    #[serde(rename = "open")]
    open: Orders,
}

#[derive(Serialize, Deserialize)]
pub struct QuoteClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: QuoteProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    quote_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct QuoteProperties {
    #[serde(rename = "data")]
    data: NseInfy,

    #[serde(rename = "status")]
    status: Orders,
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
