// This source has been automatically generated by quicktype.
// ( https://github.com/quicktype/quicktype )
//
// To use this code, simply import it into your project as a Pike module.
// To JSON-encode your object, you can pass it to `Standards.JSON.encode`
// or call `encode_json` on it.
//
// To decode a JSON string, first pass it to `Standards.JSON.decode`,
// and then pass the result to `<YourClass>_from_JSON`.
// It will return an instance of <YourClass>.
// Bear in mind that these functions have unexpected behavior,
// and will likely throw an error, if the JSON string does not
// match the expected interface, even if the JSON itself is valid.

class GttGetOrder {
    string      ref;         // json: "$ref"
    string      schema;      // json: "$schema"
    Definitions definitions; // json: "definitions"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
            "$schema" : schema,
            "definitions" : definitions,
        ]);

        return Standards.JSON.encode(json);
    }
}

GttGetOrder GttGetOrder_from_JSON(mixed json) {
    GttGetOrder retval = GttGetOrder();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    Condition        condition;     // json: "Condition"
    Data             data;          // json: "Data"
    GttGetOrderClass gtt_get_order; // json: "GttGetOrder"
    Order            order;         // json: "Order"
    OrderResult      order_result;  // json: "OrderResult"
    ResultClass      result;        // json: "Result"

    string encode_json() {
        mapping(string:mixed) json = ([
            "Condition" : condition,
            "Data" : data,
            "GttGetOrder" : gtt_get_order,
            "Order" : order,
            "OrderResult" : order_result,
            "Result" : result,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.condition = json["Condition"];
    retval.data = json["Data"];
    retval.gtt_get_order = json["GttGetOrder"];
    retval.order = json["Order"];
    retval.order_result = json["OrderResult"];
    retval.result = json["Result"];

    return retval;
}

class Condition {
    bool                additional_properties; // json: "additionalProperties"
    ConditionProperties properties;            // json: "properties"
    array(string)       required;              // json: "required"
    string              title;                 // json: "title"
    string              type;                  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additionalProperties" : additional_properties,
            "properties" : properties,
            "required" : required,
            "title" : title,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Condition Condition_from_JSON(mixed json) {
    Condition retval = Condition();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class ConditionProperties {
    Exchange      exchange;         // json: "exchange"
    Exchange      instrument_token; // json: "instrument_token"
    Exchange      last_price;       // json: "last_price"
    Exchange      tradingsymbol;    // json: "tradingsymbol"
    TriggerValues trigger_values;   // json: "trigger_values"

    string encode_json() {
        mapping(string:mixed) json = ([
            "exchange" : exchange,
            "instrument_token" : instrument_token,
            "last_price" : last_price,
            "tradingsymbol" : tradingsymbol,
            "trigger_values" : trigger_values,
        ]);

        return Standards.JSON.encode(json);
    }
}

ConditionProperties ConditionProperties_from_JSON(mixed json) {
    ConditionProperties retval = ConditionProperties();

    retval.exchange = json["exchange"];
    retval.instrument_token = json["instrument_token"];
    retval.last_price = json["last_price"];
    retval.tradingsymbol = json["tradingsymbol"];
    retval.trigger_values = json["trigger_values"];

    return retval;
}

class Exchange {
    Type type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Exchange Exchange_from_JSON(mixed json) {
    Exchange retval = Exchange();

    retval.type = json["type"];

    return retval;
}

enum Type {
    INTEGER = "integer", // json: "integer"
    NULL = "null",       // json: "null"
    NUMBER = "number",   // json: "number"
    STRING = "string",   // json: "string"
}

class TriggerValues {
    Exchange items; // json: "items"
    string   type;  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "items" : items,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

TriggerValues TriggerValues_from_JSON(mixed json) {
    TriggerValues retval = TriggerValues();

    retval.items = json["items"];
    retval.type = json["type"];

    return retval;
}

class Data {
    bool           additional_properties; // json: "additionalProperties"
    DataProperties properties;            // json: "properties"
    array(string)  required;              // json: "required"
    string         title;                 // json: "title"
    string         type;                  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additionalProperties" : additional_properties,
            "properties" : properties,
            "required" : required,
            "title" : title,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Data Data_from_JSON(mixed json) {
    Data retval = Data();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class DataProperties {
    ConditionClass condition;      // json: "condition"
    CreatedAt      created_at;     // json: "created_at"
    CreatedAt      expires_at;     // json: "expires_at"
    Exchange       id;             // json: "id"
    Exchange       meta;           // json: "meta"
    Orders         orders;         // json: "orders"
    Exchange       parent_trigger; // json: "parent_trigger"
    Exchange       status;         // json: "status"
    Exchange       type;           // json: "type"
    CreatedAt      updated_at;     // json: "updated_at"
    Exchange       user_id;        // json: "user_id"

    string encode_json() {
        mapping(string:mixed) json = ([
            "condition" : condition,
            "created_at" : created_at,
            "expires_at" : expires_at,
            "id" : id,
            "meta" : meta,
            "orders" : orders,
            "parent_trigger" : parent_trigger,
            "status" : status,
            "type" : type,
            "updated_at" : updated_at,
            "user_id" : user_id,
        ]);

        return Standards.JSON.encode(json);
    }
}

DataProperties DataProperties_from_JSON(mixed json) {
    DataProperties retval = DataProperties();

    retval.condition = json["condition"];
    retval.created_at = json["created_at"];
    retval.expires_at = json["expires_at"];
    retval.id = json["id"];
    retval.meta = json["meta"];
    retval.orders = json["orders"];
    retval.parent_trigger = json["parent_trigger"];
    retval.status = json["status"];
    retval.type = json["type"];
    retval.updated_at = json["updated_at"];
    retval.user_id = json["user_id"];

    return retval;
}

class ConditionClass {
    string ref; // json: "$ref"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
        ]);

        return Standards.JSON.encode(json);
    }
}

ConditionClass ConditionClass_from_JSON(mixed json) {
    ConditionClass retval = ConditionClass();

    retval.ref = json["$ref"];

    return retval;
}

class CreatedAt {
    string format; // json: "format"
    Type   type;   // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "format" : format,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

CreatedAt CreatedAt_from_JSON(mixed json) {
    CreatedAt retval = CreatedAt();

    retval.format = json["format"];
    retval.type = json["type"];

    return retval;
}

class Orders {
    ConditionClass items; // json: "items"
    string         type;  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "items" : items,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Orders Orders_from_JSON(mixed json) {
    Orders retval = Orders();

    retval.items = json["items"];
    retval.type = json["type"];

    return retval;
}

class GttGetOrderClass {
    bool                  additional_properties; // json: "additionalProperties"
    GttGetOrderProperties properties;            // json: "properties"
    array(string)         required;              // json: "required"
    string                title;                 // json: "title"
    string                type;                  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additionalProperties" : additional_properties,
            "properties" : properties,
            "required" : required,
            "title" : title,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

GttGetOrderClass GttGetOrderClass_from_JSON(mixed json) {
    GttGetOrderClass retval = GttGetOrderClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class GttGetOrderProperties {
    ConditionClass data;   // json: "data"
    Exchange       status; // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "data" : data,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

GttGetOrderProperties GttGetOrderProperties_from_JSON(mixed json) {
    GttGetOrderProperties retval = GttGetOrderProperties();

    retval.data = json["data"];
    retval.status = json["status"];

    return retval;
}

class Order {
    bool            additional_properties; // json: "additionalProperties"
    OrderProperties properties;            // json: "properties"
    array(string)   required;              // json: "required"
    string          title;                 // json: "title"
    string          type;                  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additionalProperties" : additional_properties,
            "properties" : properties,
            "required" : required,
            "title" : title,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Order Order_from_JSON(mixed json) {
    Order retval = Order();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class OrderProperties {
    Exchange exchange;         // json: "exchange"
    Exchange order_type;       // json: "order_type"
    Exchange price;            // json: "price"
    Exchange product;          // json: "product"
    Exchange quantity;         // json: "quantity"
    Result   result;           // json: "result"
    Exchange tradingsymbol;    // json: "tradingsymbol"
    Exchange transaction_type; // json: "transaction_type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "exchange" : exchange,
            "order_type" : order_type,
            "price" : price,
            "product" : product,
            "quantity" : quantity,
            "result" : result,
            "tradingsymbol" : tradingsymbol,
            "transaction_type" : transaction_type,
        ]);

        return Standards.JSON.encode(json);
    }
}

OrderProperties OrderProperties_from_JSON(mixed json) {
    OrderProperties retval = OrderProperties();

    retval.exchange = json["exchange"];
    retval.order_type = json["order_type"];
    retval.price = json["price"];
    retval.product = json["product"];
    retval.quantity = json["quantity"];
    retval.result = json["result"];
    retval.tradingsymbol = json["tradingsymbol"];
    retval.transaction_type = json["transaction_type"];

    return retval;
}

class Result {
    array(AnyOf) any_of; // json: "anyOf"

    string encode_json() {
        mapping(string:mixed) json = ([
            "anyOf" : any_of,
        ]);

        return Standards.JSON.encode(json);
    }
}

Result Result_from_JSON(mixed json) {
    Result retval = Result();

    retval.any_of = json["anyOf"];

    return retval;
}

class AnyOf {
    mixed|string ref;  // json: "$ref"
    Type|mixed   type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

AnyOf AnyOf_from_JSON(mixed json) {
    AnyOf retval = AnyOf();

    retval.ref = json["$ref"];
    retval.type = json["type"];

    return retval;
}

class OrderResult {
    bool                  additional_properties; // json: "additionalProperties"
    OrderResultProperties properties;            // json: "properties"
    array(string)         required;              // json: "required"
    string                title;                 // json: "title"
    string                type;                  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additionalProperties" : additional_properties,
            "properties" : properties,
            "required" : required,
            "title" : title,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

OrderResult OrderResult_from_JSON(mixed json) {
    OrderResult retval = OrderResult();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class OrderResultProperties {
    Exchange order_id;         // json: "order_id"
    Exchange rejection_reason; // json: "rejection_reason"
    Exchange status;           // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "order_id" : order_id,
            "rejection_reason" : rejection_reason,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

OrderResultProperties OrderResultProperties_from_JSON(mixed json) {
    OrderResultProperties retval = OrderResultProperties();

    retval.order_id = json["order_id"];
    retval.rejection_reason = json["rejection_reason"];
    retval.status = json["status"];

    return retval;
}

class ResultClass {
    bool             additional_properties; // json: "additionalProperties"
    ResultProperties properties;            // json: "properties"
    array(string)    required;              // json: "required"
    string           title;                 // json: "title"
    string           type;                  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additionalProperties" : additional_properties,
            "properties" : properties,
            "required" : required,
            "title" : title,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

ResultClass ResultClass_from_JSON(mixed json) {
    ResultClass retval = ResultClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class ResultProperties {
    Exchange       account_id;       // json: "account_id"
    Exchange       exchange;         // json: "exchange"
    Exchange       meta;             // json: "meta"
    ConditionClass order_result;     // json: "order_result"
    Exchange       order_type;       // json: "order_type"
    Exchange       price;            // json: "price"
    Exchange       product;          // json: "product"
    Exchange       quantity;         // json: "quantity"
    CreatedAt      timestamp;        // json: "timestamp"
    Exchange       tradingsymbol;    // json: "tradingsymbol"
    Exchange       transaction_type; // json: "transaction_type"
    Exchange       triggered_at;     // json: "triggered_at"
    Exchange       validity;         // json: "validity"

    string encode_json() {
        mapping(string:mixed) json = ([
            "account_id" : account_id,
            "exchange" : exchange,
            "meta" : meta,
            "order_result" : order_result,
            "order_type" : order_type,
            "price" : price,
            "product" : product,
            "quantity" : quantity,
            "timestamp" : timestamp,
            "tradingsymbol" : tradingsymbol,
            "transaction_type" : transaction_type,
            "triggered_at" : triggered_at,
            "validity" : validity,
        ]);

        return Standards.JSON.encode(json);
    }
}

ResultProperties ResultProperties_from_JSON(mixed json) {
    ResultProperties retval = ResultProperties();

    retval.account_id = json["account_id"];
    retval.exchange = json["exchange"];
    retval.meta = json["meta"];
    retval.order_result = json["order_result"];
    retval.order_type = json["order_type"];
    retval.price = json["price"];
    retval.product = json["product"];
    retval.quantity = json["quantity"];
    retval.timestamp = json["timestamp"];
    retval.tradingsymbol = json["tradingsymbol"];
    retval.transaction_type = json["transaction_type"];
    retval.triggered_at = json["triggered_at"];
    retval.validity = json["validity"];

    return retval;
}
