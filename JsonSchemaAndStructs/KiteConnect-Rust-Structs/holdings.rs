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
pub struct Holdings {
    #[serde(rename = "$ref")]
    holdings_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Datum")]
    datum: Datum,

    #[serde(rename = "Holdings")]
    holdings: HoldingsClass,
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
    #[serde(rename = "authorised_date")]
    authorised_date: AuthorisedDate,

    #[serde(rename = "authorised_quantity")]
    authorised_quantity: AuthorisedQuantity,

    #[serde(rename = "average_price")]
    average_price: AuthorisedQuantity,

    #[serde(rename = "close_price")]
    close_price: AuthorisedQuantity,

    #[serde(rename = "collateral_quantity")]
    collateral_quantity: AuthorisedQuantity,

    #[serde(rename = "collateral_type")]
    collateral_type: AuthorisedQuantity,

    #[serde(rename = "day_change")]
    day_change: AuthorisedQuantity,

    #[serde(rename = "day_change_percentage")]
    day_change_percentage: AuthorisedQuantity,

    #[serde(rename = "discrepancy")]
    discrepancy: AuthorisedQuantity,

    #[serde(rename = "exchange")]
    exchange: AuthorisedQuantity,

    #[serde(rename = "instrument_token")]
    instrument_token: AuthorisedQuantity,

    #[serde(rename = "isin")]
    isin: AuthorisedQuantity,

    #[serde(rename = "last_price")]
    last_price: AuthorisedQuantity,

    #[serde(rename = "opening_quantity")]
    opening_quantity: AuthorisedQuantity,

    #[serde(rename = "pnl")]
    pnl: AuthorisedQuantity,

    #[serde(rename = "price")]
    price: AuthorisedQuantity,

    #[serde(rename = "product")]
    product: AuthorisedQuantity,

    #[serde(rename = "quantity")]
    quantity: AuthorisedQuantity,

    #[serde(rename = "realised_quantity")]
    realised_quantity: AuthorisedQuantity,

    #[serde(rename = "t1_quantity")]
    t1_quantity: AuthorisedQuantity,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: AuthorisedQuantity,

    #[serde(rename = "used_quantity")]
    used_quantity: AuthorisedQuantity,
}

#[derive(Serialize, Deserialize)]
pub struct AuthorisedDate {
    #[serde(rename = "format")]
    format: String,

    #[serde(rename = "type")]
    authorised_date_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct AuthorisedQuantity {
    #[serde(rename = "type")]
    authorised_quantity_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct HoldingsClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: HoldingsProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    holdings_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct HoldingsProperties {
    #[serde(rename = "data")]
    data: Data,

    #[serde(rename = "status")]
    status: AuthorisedQuantity,
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
    #[serde(rename = "boolean")]
    Boolean,

    #[serde(rename = "integer")]
    Integer,

    #[serde(rename = "number")]
    Number,

    #[serde(rename = "string")]
    String,
}
