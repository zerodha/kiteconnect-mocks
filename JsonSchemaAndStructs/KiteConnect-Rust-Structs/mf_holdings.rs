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
pub struct MfHoldings {
    #[serde(rename = "$ref")]
    mf_holdings_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Datum")]
    datum: Datum,

    #[serde(rename = "MFHoldings")]
    mf_holdings: MfHoldingsClass,
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

    #[serde(rename = "folio")]
    folio: AveragePrice,

    #[serde(rename = "fund")]
    fund: AveragePrice,

    #[serde(rename = "last_price")]
    last_price: AveragePrice,

    #[serde(rename = "last_price_date")]
    last_price_date: AveragePrice,

    #[serde(rename = "pledged_quantity")]
    pledged_quantity: AveragePrice,

    #[serde(rename = "pnl")]
    pnl: AveragePrice,

    #[serde(rename = "quantity")]
    quantity: AveragePrice,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: AveragePrice,
}

#[derive(Serialize, Deserialize)]
pub struct AveragePrice {
    #[serde(rename = "type")]
    average_price_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct MfHoldingsClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MfHoldingsProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    mf_holdings_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfHoldingsProperties {
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
