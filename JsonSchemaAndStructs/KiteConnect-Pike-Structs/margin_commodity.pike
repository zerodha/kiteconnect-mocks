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

class MarginCommodity {
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

MarginCommodity MarginCommodity_from_JSON(mixed json) {
    MarginCommodity retval = MarginCommodity();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    Available            available;        // json: "Available"
    Data                 data;             // json: "Data"
    MarginCommodityClass margin_commodity; // json: "MarginCommodity"

    string encode_json() {
        mapping(string:mixed) json = ([
            "Available" : available,
            "Data" : data,
            "MarginCommodity" : margin_commodity,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.available = json["Available"];
    retval.data = json["Data"];
    retval.margin_commodity = json["MarginCommodity"];

    return retval;
}

class Available {
    bool                additional_properties; // json: "additionalProperties"
    AvailableProperties properties;            // json: "properties"
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

Available Available_from_JSON(mixed json) {
    Available retval = Available();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class AvailableProperties {
    AdhocMargin adhoc_margin;    // json: "adhoc_margin"
    AdhocMargin cash;            // json: "cash"
    AdhocMargin collateral;      // json: "collateral"
    AdhocMargin intraday_payin;  // json: "intraday_payin"
    AdhocMargin live_balance;    // json: "live_balance"
    AdhocMargin opening_balance; // json: "opening_balance"

    string encode_json() {
        mapping(string:mixed) json = ([
            "adhoc_margin" : adhoc_margin,
            "cash" : cash,
            "collateral" : collateral,
            "intraday_payin" : intraday_payin,
            "live_balance" : live_balance,
            "opening_balance" : opening_balance,
        ]);

        return Standards.JSON.encode(json);
    }
}

AvailableProperties AvailableProperties_from_JSON(mixed json) {
    AvailableProperties retval = AvailableProperties();

    retval.adhoc_margin = json["adhoc_margin"];
    retval.cash = json["cash"];
    retval.collateral = json["collateral"];
    retval.intraday_payin = json["intraday_payin"];
    retval.live_balance = json["live_balance"];
    retval.opening_balance = json["opening_balance"];

    return retval;
}

class AdhocMargin {
    string type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

AdhocMargin AdhocMargin_from_JSON(mixed json) {
    AdhocMargin retval = AdhocMargin();

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
    AvailableClass available; // json: "available"
    AdhocMargin    enabled;   // json: "enabled"
    AdhocMargin    net;       // json: "net"
    Utilised       utilised;  // json: "utilised"

    string encode_json() {
        mapping(string:mixed) json = ([
            "available" : available,
            "enabled" : enabled,
            "net" : net,
            "utilised" : utilised,
        ]);

        return Standards.JSON.encode(json);
    }
}

DataProperties DataProperties_from_JSON(mixed json) {
    DataProperties retval = DataProperties();

    retval.available = json["available"];
    retval.enabled = json["enabled"];
    retval.net = json["net"];
    retval.utilised = json["utilised"];

    return retval;
}

class AvailableClass {
    string ref; // json: "$ref"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
        ]);

        return Standards.JSON.encode(json);
    }
}

AvailableClass AvailableClass_from_JSON(mixed json) {
    AvailableClass retval = AvailableClass();

    retval.ref = json["$ref"];

    return retval;
}

class Utilised {
    AdhocMargin additional_properties; // json: "additionalProperties"
    string      type;                  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additionalProperties" : additional_properties,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Utilised Utilised_from_JSON(mixed json) {
    Utilised retval = Utilised();

    retval.additional_properties = json["additionalProperties"];
    retval.type = json["type"];

    return retval;
}

class MarginCommodityClass {
    bool                      additional_properties; // json: "additionalProperties"
    MarginCommodityProperties properties;            // json: "properties"
    array(string)             required;              // json: "required"
    string                    title;                 // json: "title"
    string                    type;                  // json: "type"

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

MarginCommodityClass MarginCommodityClass_from_JSON(mixed json) {
    MarginCommodityClass retval = MarginCommodityClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class MarginCommodityProperties {
    AvailableClass data;   // json: "data"
    AdhocMargin    status; // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "data" : data,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

MarginCommodityProperties MarginCommodityProperties_from_JSON(mixed json) {
    MarginCommodityProperties retval = MarginCommodityProperties();

    retval.data = json["data"];
    retval.status = json["status"];

    return retval;
}
