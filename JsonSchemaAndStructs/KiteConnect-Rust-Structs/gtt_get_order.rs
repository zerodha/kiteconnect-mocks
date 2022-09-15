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
pub struct GttGetOrder {
    #[serde(rename = "$ref")]
    gtt_get_order_ref: String,

    #[serde(rename = "$schema")]
    schema: String,

    #[serde(rename = "definitions")]
    definitions: Definitions,
}

#[derive(Serialize, Deserialize)]
pub struct Definitions {
    #[serde(rename = "Condition")]
    condition: Condition,

    #[serde(rename = "Data")]
    data: Data,

    #[serde(rename = "GttGetOrder")]
    gtt_get_order: GttGetOrderClass,

    #[serde(rename = "Order")]
    order: Order,

    #[serde(rename = "OrderResult")]
    order_result: OrderResult,

    #[serde(rename = "Result")]
    result: ResultClass,
}

#[derive(Serialize, Deserialize)]
pub struct Condition {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: ConditionProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    condition_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct ConditionProperties {
    #[serde(rename = "exchange")]
    exchange: Exchange,

    #[serde(rename = "instrument_token")]
    instrument_token: Exchange,

    #[serde(rename = "last_price")]
    last_price: Exchange,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: Exchange,

    #[serde(rename = "trigger_values")]
    trigger_values: TriggerValues,
}

#[derive(Serialize, Deserialize)]
pub struct Exchange {
    #[serde(rename = "type")]
    exchange_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct TriggerValues {
    #[serde(rename = "items")]
    items: Exchange,

    #[serde(rename = "type")]
    trigger_values_type: String,
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
    #[serde(rename = "condition")]
    condition: ConditionClass,

    #[serde(rename = "created_at")]
    created_at: CreatedAt,

    #[serde(rename = "expires_at")]
    expires_at: CreatedAt,

    #[serde(rename = "id")]
    id: Exchange,

    #[serde(rename = "meta")]
    meta: Exchange,

    #[serde(rename = "orders")]
    orders: Orders,

    #[serde(rename = "parent_trigger")]
    parent_trigger: Exchange,

    #[serde(rename = "status")]
    status: Exchange,

    #[serde(rename = "type")]
    data_properties_type: Exchange,

    #[serde(rename = "updated_at")]
    updated_at: CreatedAt,

    #[serde(rename = "user_id")]
    user_id: Exchange,
}

#[derive(Serialize, Deserialize)]
pub struct ConditionClass {
    #[serde(rename = "$ref")]
    condition_class_ref: String,
}

#[derive(Serialize, Deserialize)]
pub struct CreatedAt {
    #[serde(rename = "format")]
    format: String,

    #[serde(rename = "type")]
    created_at_type: Type,
}

#[derive(Serialize, Deserialize)]
pub struct Orders {
    #[serde(rename = "items")]
    items: ConditionClass,

    #[serde(rename = "type")]
    orders_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct GttGetOrderClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: GttGetOrderProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    gtt_get_order_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct GttGetOrderProperties {
    #[serde(rename = "data")]
    data: ConditionClass,

    #[serde(rename = "status")]
    status: Exchange,
}

#[derive(Serialize, Deserialize)]
pub struct Order {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: OrderProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    order_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct OrderProperties {
    #[serde(rename = "exchange")]
    exchange: Exchange,

    #[serde(rename = "order_type")]
    order_type: Exchange,

    #[serde(rename = "price")]
    price: Exchange,

    #[serde(rename = "product")]
    product: Exchange,

    #[serde(rename = "quantity")]
    quantity: Exchange,

    #[serde(rename = "result")]
    result: Result,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: Exchange,

    #[serde(rename = "transaction_type")]
    transaction_type: Exchange,
}

#[derive(Serialize, Deserialize)]
pub struct Result {
    #[serde(rename = "anyOf")]
    any_of: Vec<AnyOf>,
}

#[derive(Serialize, Deserialize)]
pub struct AnyOf {
    #[serde(rename = "$ref")]
    any_of_ref: Option<String>,

    #[serde(rename = "type")]
    any_of_type: Option<Type>,
}

#[derive(Serialize, Deserialize)]
pub struct OrderResult {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: OrderResultProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    order_result_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct OrderResultProperties {
    #[serde(rename = "order_id")]
    order_id: Exchange,

    #[serde(rename = "rejection_reason")]
    rejection_reason: Exchange,

    #[serde(rename = "status")]
    status: Exchange,
}

#[derive(Serialize, Deserialize)]
pub struct ResultClass {
    #[serde(rename = "additionalProperties")]
    additional_properties: bool,

    #[serde(rename = "properties")]
    properties: ResultProperties,

    #[serde(rename = "required")]
    required: Vec<String>,

    #[serde(rename = "title")]
    title: String,

    #[serde(rename = "type")]
    result_class_type: String,
}

#[derive(Serialize, Deserialize)]
pub struct ResultProperties {
    #[serde(rename = "account_id")]
    account_id: Exchange,

    #[serde(rename = "exchange")]
    exchange: Exchange,

    #[serde(rename = "meta")]
    meta: Exchange,

    #[serde(rename = "order_result")]
    order_result: ConditionClass,

    #[serde(rename = "order_type")]
    order_type: Exchange,

    #[serde(rename = "price")]
    price: Exchange,

    #[serde(rename = "product")]
    product: Exchange,

    #[serde(rename = "quantity")]
    quantity: Exchange,

    #[serde(rename = "timestamp")]
    timestamp: CreatedAt,

    #[serde(rename = "tradingsymbol")]
    tradingsymbol: Exchange,

    #[serde(rename = "transaction_type")]
    transaction_type: Exchange,

    #[serde(rename = "triggered_at")]
    triggered_at: Exchange,

    #[serde(rename = "validity")]
    validity: Exchange,
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
