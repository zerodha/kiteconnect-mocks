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
pub struct MfSipInfo {
    #[serde(rename = "$ref")]
    mf_sip_info_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "MFSIPInfo")]
    mfsip_info: MfsipInfoClass,

    #[serde(rename = "StepUp")]
    step_up: StepUpClass,
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

    #[serde(rename = "fund_source")]
    fund_source: CompletedInstalments,

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
    sip_reg_num: CompletedInstalments,

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
    format: String,

    #[serde(rename = "type")]
    created_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct StepUp {
    #[serde(rename = "$ref")]
    step_up_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfsipInfoClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: MfsipInfoProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    mfsip_info_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct MfsipInfoProperties {
    #[serde(rename = "data")]
    data: StepUp,

    #[serde(rename = "status")]
    status: CompletedInstalments,
}

#[derive(Serialize, Deserialize)]
pub struct StepUpClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: StepUpProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    step_up_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct StepUpProperties {
    #[serde(rename = "15-02")]
    the_1502: CompletedInstalments,
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
