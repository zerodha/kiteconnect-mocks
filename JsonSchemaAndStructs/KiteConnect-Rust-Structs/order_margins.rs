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
pub struct OrderMargins {
    #[serde(rename = "$ref")]
    order_margins_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Datum")]
    datum: Datum,

    #[serde(rename = "OrderMargins")]
    order_margins: OrderMarginsClass,

    #[serde(rename = "Pnl")]
    pnl: PnlClass,
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
    #[serde(rename = "additional")]
    additional: Additional,

    #[serde(rename = "bo")]
    bo: Additional,

    #[serde(rename = "cash")]
    cash: Additional,

    #[serde(rename = "exchange")]
    exchange: Additional,

    #[serde(rename = "exposure")]
    exposure: Additional,

    #[serde(rename = "option_premium")]
    option_premium: Additional,

    #[serde(rename = "pnl")]
    pnl: Pnl,

    #[serde(rename = "span")]
    span: Additional,

    #[serde(rename = "total")]
    total: Additional,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: Additional,

    #[serde(rename = "type")]
    datum_properties_type: Additional,

    #[serde(rename = "var")]
    var: Additional,
}

#[derive(Serialize, Deserialize)]
pub struct Additional {
    #[serde(rename = "type")]
    additional_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct Pnl {
    #[serde(rename = "$ref")]
    pnl_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct OrderMarginsClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: OrderMarginsProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    order_margins_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct OrderMarginsProperties {
    #[serde(rename = "data")]
    data: Data,

    #[serde(rename = "status")]
    status: Additional,
}

#[derive(Serialize, Deserialize)]
pub struct Data {
    #[serde(rename = "items")]
    items: Pnl,

    #[serde(rename = "type")]
    data_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct PnlClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: PnlProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    pnl_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct PnlProperties {
    #[serde(rename = "realised")]
    realised: Additional,

    #[serde(rename = "unrealised")]
    unrealised: Additional,
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
