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

class Postback {
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

Postback Postback_from_JSON(mixed json) {
    Postback retval = Postback();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    Meta          meta;     // json: "Meta"
    PostbackClass postback; // json: "Postback"

    string encode_json() {
        mapping(string:mixed) json = ([
            "Meta" : meta,
            "Postback" : postback,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.meta = json["Meta"];
    retval.postback = json["Postback"];

    return retval;
}

class Meta {
    bool   additional_properties; // json: "additionalProperties"
    string title;                 // json: "title"
    string type;                  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additionalProperties" : additional_properties,
            "title" : title,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Meta Meta_from_JSON(mixed json) {
    Meta retval = Meta();

    retval.additional_properties = json["additionalProperties"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class PostbackClass {
    bool          additional_properties; // json: "additionalProperties"
    Properties    properties;            // json: "properties"
    array(string) required;              // json: "required"
    string        title;                 // json: "title"
    string        type;                  // json: "type"

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

PostbackClass PostbackClass_from_JSON(mixed json) {
    PostbackClass retval = PostbackClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class Properties {
    AppId     app_id;                    // json: "app_id"
    AppId     average_price;             // json: "average_price"
    AppId     cancelled_quantity;        // json: "cancelled_quantity"
    AppId     checksum;                  // json: "checksum"
    AppId     disclosed_quantity;        // json: "disclosed_quantity"
    AppId     exchange;                  // json: "exchange"
    AppId     exchange_order_id;         // json: "exchange_order_id"
    Timestamp exchange_timestamp;        // json: "exchange_timestamp"
    Timestamp exchange_update_timestamp; // json: "exchange_update_timestamp"
    AppId     filled_quantity;           // json: "filled_quantity"
    AppId     guid;                      // json: "guid"
    AppId     instrument_token;          // json: "instrument_token"
    AppId     market_protection;         // json: "market_protection"
    MetaClass meta;                      // json: "meta"
    AppId     order_id;                  // json: "order_id"
    Timestamp order_timestamp;           // json: "order_timestamp"
    AppId     order_type;                // json: "order_type"
    AppId     parent_order_id;           // json: "parent_order_id"
    AppId     pending_quantity;          // json: "pending_quantity"
    AppId     placed_by;                 // json: "placed_by"
    AppId     price;                     // json: "price"
    AppId     product;                   // json: "product"
    AppId     quantity;                  // json: "quantity"
    AppId     status;                    // json: "status"
    AppId     status_message;            // json: "status_message"
    AppId     status_message_raw;        // json: "status_message_raw"
    AppId     tag;                       // json: "tag"
    AppId     tradingsymbol;             // json: "tradingsymbol"
    AppId     transaction_type;          // json: "transaction_type"
    AppId     trigger_price;             // json: "trigger_price"
    AppId     unfilled_quantity;         // json: "unfilled_quantity"
    AppId     user_id;                   // json: "user_id"
    AppId     validity;                  // json: "validity"
    AppId     variety;                   // json: "variety"

    string encode_json() {
        mapping(string:mixed) json = ([
            "app_id" : app_id,
            "average_price" : average_price,
            "cancelled_quantity" : cancelled_quantity,
            "checksum" : checksum,
            "disclosed_quantity" : disclosed_quantity,
            "exchange" : exchange,
            "exchange_order_id" : exchange_order_id,
            "exchange_timestamp" : exchange_timestamp,
            "exchange_update_timestamp" : exchange_update_timestamp,
            "filled_quantity" : filled_quantity,
            "guid" : guid,
            "instrument_token" : instrument_token,
            "market_protection" : market_protection,
            "meta" : meta,
            "order_id" : order_id,
            "order_timestamp" : order_timestamp,
            "order_type" : order_type,
            "parent_order_id" : parent_order_id,
            "pending_quantity" : pending_quantity,
            "placed_by" : placed_by,
            "price" : price,
            "product" : product,
            "quantity" : quantity,
            "status" : status,
            "status_message" : status_message,
            "status_message_raw" : status_message_raw,
            "tag" : tag,
            "tradingsymbol" : tradingsymbol,
            "transaction_type" : transaction_type,
            "trigger_price" : trigger_price,
            "unfilled_quantity" : unfilled_quantity,
            "user_id" : user_id,
            "validity" : validity,
            "variety" : variety,
        ]);

        return Standards.JSON.encode(json);
    }
}

Properties Properties_from_JSON(mixed json) {
    Properties retval = Properties();

    retval.app_id = json["app_id"];
    retval.average_price = json["average_price"];
    retval.cancelled_quantity = json["cancelled_quantity"];
    retval.checksum = json["checksum"];
    retval.disclosed_quantity = json["disclosed_quantity"];
    retval.exchange = json["exchange"];
    retval.exchange_order_id = json["exchange_order_id"];
    retval.exchange_timestamp = json["exchange_timestamp"];
    retval.exchange_update_timestamp = json["exchange_update_timestamp"];
    retval.filled_quantity = json["filled_quantity"];
    retval.guid = json["guid"];
    retval.instrument_token = json["instrument_token"];
    retval.market_protection = json["market_protection"];
    retval.meta = json["meta"];
    retval.order_id = json["order_id"];
    retval.order_timestamp = json["order_timestamp"];
    retval.order_type = json["order_type"];
    retval.parent_order_id = json["parent_order_id"];
    retval.pending_quantity = json["pending_quantity"];
    retval.placed_by = json["placed_by"];
    retval.price = json["price"];
    retval.product = json["product"];
    retval.quantity = json["quantity"];
    retval.status = json["status"];
    retval.status_message = json["status_message"];
    retval.status_message_raw = json["status_message_raw"];
    retval.tag = json["tag"];
    retval.tradingsymbol = json["tradingsymbol"];
    retval.transaction_type = json["transaction_type"];
    retval.trigger_price = json["trigger_price"];
    retval.unfilled_quantity = json["unfilled_quantity"];
    retval.user_id = json["user_id"];
    retval.validity = json["validity"];
    retval.variety = json["variety"];

    return retval;
}

class AppId {
    Type type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

AppId AppId_from_JSON(mixed json) {
    AppId retval = AppId();

    retval.type = json["type"];

    return retval;
}

enum Type {
    INTEGER = "integer", // json: "integer"
    NULL = "null",       // json: "null"
    STRING = "string",   // json: "string"
}

class Timestamp {
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

Timestamp Timestamp_from_JSON(mixed json) {
    Timestamp retval = Timestamp();

    retval.format = json["format"];
    retval.type = json["type"];

    return retval;
}

class MetaClass {
    string ref; // json: "$ref"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
        ]);

        return Standards.JSON.encode(json);
    }
}

MetaClass MetaClass_from_JSON(mixed json) {
    MetaClass retval = MetaClass();

    retval.ref = json["$ref"];

    return retval;
}
