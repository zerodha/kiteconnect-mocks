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
pub struct MfSips {
    #[serde(rename = "$ref")]
    mf_sips_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Datum")]
    datum: Datum,

    #[serde(rename = "MFSips")]
    mf_sips: MfSipsClass,
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
    #[serde(rename = "completed_instalments")]
    completed_instalments: CompletedInstalments,

    #[serde(rename = "created")]
    created: Created,

    #[serde(rename = "dividend_type")]
    dividend_type: CompletedInstalments,

    #[serde(rename = "frequency")]
    frequency: CompletedInstalments,

    #[serde(rename = "fund")]
    fund: CompletedInstalments,

    #[serde(rename = "instalment_amount")]
    instalment_amount: CompletedInstalments,

    #[serde(rename = "instalment_day")]
    instalment_day: CompletedInstalments,

    #[serde(rename = "instalments")]
    instalments: CompletedInstalments,

    #[serde(rename = "last_instalment")]
    last_instalment: Created,

    #[serde(rename = "next_instalment")]
    next_instalment: Created,

    #[serde(rename = "pending_instalments")]
    pending_instalments: CompletedInstalments,

    #[serde(rename = "sip_id")]
    sip_id: CompletedInstalments,

    #[serde(rename = "sip_reg_num")]
    sip_reg_num: SipRegNum,

    #[serde(rename = "sip_type")]
    sip_type: CompletedInstalments,

    #[serde(rename = "status")]
    status: CompletedInstalments,

    #[serde(rename = "step_up")]
    step_up: StepUp,

    #[serde(rename = "tag")]
    tag: CompletedInstalments,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: CompletedInstalments,

    #[serde(rename = "transaction_type")]
    transaction_type: CompletedInstalments,

    #[serde(rename = "trigger_price")]
    trigger_price: CompletedInstalments,
}

#[derive(Serialize, Deserialize)]
pub struct CompletedInstalments {
    #[serde(rename = "type")]
    completed_instalments_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct Created {
    #[serde(rename = "format")]
    format: Option<String>,

    #[serde(rename = "type")]
    created_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct SipRegNum {
    #[serde(rename = "anyOf")]
    any_of: Vec<Created>,
}

#[derive(Serialize, Deserialize)]
pub struct StepUp {
    #[serde(rename = "additionalProperties")]
    additional_properties: CompletedInstalments,

    #[serde(rename = "type")]
    step_up_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfSipsClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MfSipsProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    mf_sips_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfSipsProperties {
    #[serde(rename = "data")]
    data: Data,
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
