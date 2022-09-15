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
pub struct BasketMargins {
    #[serde(rename = "$ref")]
    basket_margins_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "BasketMargins")]
    basket_margins: BasketMarginsClass,

    #[serde(rename = "Data")]
    data: DataClass,

    #[serde(rename = "Final")]
    definitions_final: Final,

    #[serde(rename = "Pnl")]
    pnl: Pnl,
}

#[derive(Serialize, Deserialize)]
pub struct BasketMarginsClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: BasketMarginsProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    basket_margins_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct BasketMarginsProperties {
    #[serde(rename = "data")]
    data: Data,

    #[serde(rename = "status")]
    status: Status,
}

#[derive(Serialize, Deserialize)]
pub struct Data {
    #[serde(rename = "$ref")]
    data_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct Status {
    #[serde(rename = "type")]
    status_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct DataClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: DataProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    data_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct DataProperties {
    #[serde(rename = "final")]
    data_properties_final: Data,

    #[serde(rename = "initial")]
    initial: Data,

    #[serde(rename = "orders")]
    orders: Orders,
}

#[derive(Serialize, Deserialize)]
pub struct Orders {
    #[serde(rename = "items")]
    items: Data,

    #[serde(rename = "type")]
    orders_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct Final {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: FinalProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    final_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct FinalProperties {
    #[serde(rename = "additional")]
    additional: Status,

    #[serde(rename = "bo")]
    bo: Status,

    #[serde(rename = "cash")]
    cash: Status,

    #[serde(rename = "exchange")]
    exchange: Status,

    #[serde(rename = "exposure")]
    exposure: Status,

    #[serde(rename = "option_premium")]
    option_premium: Status,

    #[serde(rename = "pnl")]
    pnl: Data,

    #[serde(rename = "span")]
    span: Status,

    #[serde(rename = "total")]
    total: Status,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: Status,

    #[serde(rename = "type")]
    final_properties_type: Status,

    #[serde(rename = "var")]
    var: Status,
}

#[derive(Serialize, Deserialize)]
pub struct Pnl {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: PnlProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    pnl_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct PnlProperties {
    #[serde(rename = "realised")]
    realised: Status,

    #[serde(rename = "unrealised")]
    unrealised: Status,
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
