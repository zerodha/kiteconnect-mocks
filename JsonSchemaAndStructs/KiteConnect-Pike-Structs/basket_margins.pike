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

class BasketMargins {
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

BasketMargins BasketMargins_from_JSON(mixed json) {
    BasketMargins retval = BasketMargins();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    BasketMarginsClass basket_margins;    // json: "BasketMargins"
    DataClass          data;              // json: "Data"
    Final              definitions_final; // json: "Final"
    Pnl                pnl;               // json: "Pnl"

    string encode_json() {
        mapping(string:mixed) json = ([
            "BasketMargins" : basket_margins,
            "Data" : data,
            "Final" : definitions_final,
            "Pnl" : pnl,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.basket_margins = json["BasketMargins"];
    retval.data = json["Data"];
    retval.definitions_final = json["Final"];
    retval.pnl = json["Pnl"];

    return retval;
}

class BasketMarginsClass {
    bool                    additional_properties; // json: "additionalProperties"
    BasketMarginsProperties properties;            // json: "properties"
    array(string)           required;              // json: "required"
    string                  title;                 // json: "title"
    string                  type;                  // json: "type"

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

BasketMarginsClass BasketMarginsClass_from_JSON(mixed json) {
    BasketMarginsClass retval = BasketMarginsClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class BasketMarginsProperties {
    Data   data;   // json: "data"
    Status status; // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "data" : data,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

BasketMarginsProperties BasketMarginsProperties_from_JSON(mixed json) {
    BasketMarginsProperties retval = BasketMarginsProperties();

    retval.data = json["data"];
    retval.status = json["status"];

    return retval;
}

class Data {
    string ref; // json: "$ref"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
        ]);

        return Standards.JSON.encode(json);
    }
}

Data Data_from_JSON(mixed json) {
    Data retval = Data();

    retval.ref = json["$ref"];

    return retval;
}

class Status {
    Type type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Status Status_from_JSON(mixed json) {
    Status retval = Status();

    retval.type = json["type"];

    return retval;
}

enum Type {
    INTEGER = "integer", // json: "integer"
    NUMBER = "number",   // json: "number"
    STRING = "string",   // json: "string"
}

class DataClass {
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

DataClass DataClass_from_JSON(mixed json) {
    DataClass retval = DataClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class DataProperties {
    Data   data_properties_final; // json: "final"
    Data   initial;               // json: "initial"
    Orders orders;                // json: "orders"

    string encode_json() {
        mapping(string:mixed) json = ([
            "final" : data_properties_final,
            "initial" : initial,
            "orders" : orders,
        ]);

        return Standards.JSON.encode(json);
    }
}

DataProperties DataProperties_from_JSON(mixed json) {
    DataProperties retval = DataProperties();

    retval.data_properties_final = json["final"];
    retval.initial = json["initial"];
    retval.orders = json["orders"];

    return retval;
}

class Orders {
    Data   items; // json: "items"
    string type;  // json: "type"

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

class Final {
    bool            additional_properties; // json: "additionalProperties"
    FinalProperties properties;            // json: "properties"
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

Final Final_from_JSON(mixed json) {
    Final retval = Final();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class FinalProperties {
    Status additional;     // json: "additional"
    Status bo;             // json: "bo"
    Status cash;           // json: "cash"
    Status exchange;       // json: "exchange"
    Status exposure;       // json: "exposure"
    Status option_premium; // json: "option_premium"
    Data   pnl;            // json: "pnl"
    Status span;           // json: "span"
    Status total;          // json: "total"
    Status tradingsymbol;  // json: "tradingsymbol"
    Status type;           // json: "type"
    Status var;            // json: "var"

    string encode_json() {
        mapping(string:mixed) json = ([
            "additional" : additional,
            "bo" : bo,
            "cash" : cash,
            "exchange" : exchange,
            "exposure" : exposure,
            "option_premium" : option_premium,
            "pnl" : pnl,
            "span" : span,
            "total" : total,
            "tradingsymbol" : tradingsymbol,
            "type" : type,
            "var" : var,
        ]);

        return Standards.JSON.encode(json);
    }
}

FinalProperties FinalProperties_from_JSON(mixed json) {
    FinalProperties retval = FinalProperties();

    retval.additional = json["additional"];
    retval.bo = json["bo"];
    retval.cash = json["cash"];
    retval.exchange = json["exchange"];
    retval.exposure = json["exposure"];
    retval.option_premium = json["option_premium"];
    retval.pnl = json["pnl"];
    retval.span = json["span"];
    retval.total = json["total"];
    retval.tradingsymbol = json["tradingsymbol"];
    retval.type = json["type"];
    retval.var = json["var"];

    return retval;
}

class Pnl {
    bool          additional_properties; // json: "additionalProperties"
    PnlProperties properties;            // json: "properties"
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

Pnl Pnl_from_JSON(mixed json) {
    Pnl retval = Pnl();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class PnlProperties {
    Status realised;   // json: "realised"
    Status unrealised; // json: "unrealised"

    string encode_json() {
        mapping(string:mixed) json = ([
            "realised" : realised,
            "unrealised" : unrealised,
        ]);

        return Standards.JSON.encode(json);
    }
}

PnlProperties PnlProperties_from_JSON(mixed json) {
    PnlProperties retval = PnlProperties();

    retval.realised = json["realised"];
    retval.unrealised = json["unrealised"];

    return retval;
}