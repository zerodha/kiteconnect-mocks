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

class Holdings {
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

Holdings Holdings_from_JSON(mixed json) {
    Holdings retval = Holdings();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    Datum         datum;    // json: "Datum"
    HoldingsClass holdings; // json: "Holdings"

    string encode_json() {
        mapping(string:mixed) json = ([
            "Datum" : datum,
            "Holdings" : holdings,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.datum = json["Datum"];
    retval.holdings = json["Holdings"];

    return retval;
}

class Datum {
    bool            additional_properties; // json: "additionalProperties"
    DatumProperties properties;            // json: "properties"
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

Datum Datum_from_JSON(mixed json) {
    Datum retval = Datum();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class DatumProperties {
    AuthorisedDate     authorised_date;       // json: "authorised_date"
    AuthorisedQuantity authorised_quantity;   // json: "authorised_quantity"
    AuthorisedQuantity average_price;         // json: "average_price"
    AuthorisedQuantity close_price;           // json: "close_price"
    AuthorisedQuantity collateral_quantity;   // json: "collateral_quantity"
    AuthorisedQuantity collateral_type;       // json: "collateral_type"
    AuthorisedQuantity day_change;            // json: "day_change"
    AuthorisedQuantity day_change_percentage; // json: "day_change_percentage"
    AuthorisedQuantity discrepancy;           // json: "discrepancy"
    AuthorisedQuantity exchange;              // json: "exchange"
    AuthorisedQuantity instrument_token;      // json: "instrument_token"
    AuthorisedQuantity isin;                  // json: "isin"
    AuthorisedQuantity last_price;            // json: "last_price"
    AuthorisedQuantity opening_quantity;      // json: "opening_quantity"
    AuthorisedQuantity pnl;                   // json: "pnl"
    AuthorisedQuantity price;                 // json: "price"
    AuthorisedQuantity product;               // json: "product"
    AuthorisedQuantity quantity;              // json: "quantity"
    AuthorisedQuantity realised_quantity;     // json: "realised_quantity"
    AuthorisedQuantity t1_quantity;           // json: "t1_quantity"
    AuthorisedQuantity tradingsymbol;         // json: "tradingsymbol"
    AuthorisedQuantity used_quantity;         // json: "used_quantity"

    string encode_json() {
        mapping(string:mixed) json = ([
            "authorised_date" : authorised_date,
            "authorised_quantity" : authorised_quantity,
            "average_price" : average_price,
            "close_price" : close_price,
            "collateral_quantity" : collateral_quantity,
            "collateral_type" : collateral_type,
            "day_change" : day_change,
            "day_change_percentage" : day_change_percentage,
            "discrepancy" : discrepancy,
            "exchange" : exchange,
            "instrument_token" : instrument_token,
            "isin" : isin,
            "last_price" : last_price,
            "opening_quantity" : opening_quantity,
            "pnl" : pnl,
            "price" : price,
            "product" : product,
            "quantity" : quantity,
            "realised_quantity" : realised_quantity,
            "t1_quantity" : t1_quantity,
            "tradingsymbol" : tradingsymbol,
            "used_quantity" : used_quantity,
        ]);

        return Standards.JSON.encode(json);
    }
}

DatumProperties DatumProperties_from_JSON(mixed json) {
    DatumProperties retval = DatumProperties();

    retval.authorised_date = json["authorised_date"];
    retval.authorised_quantity = json["authorised_quantity"];
    retval.average_price = json["average_price"];
    retval.close_price = json["close_price"];
    retval.collateral_quantity = json["collateral_quantity"];
    retval.collateral_type = json["collateral_type"];
    retval.day_change = json["day_change"];
    retval.day_change_percentage = json["day_change_percentage"];
    retval.discrepancy = json["discrepancy"];
    retval.exchange = json["exchange"];
    retval.instrument_token = json["instrument_token"];
    retval.isin = json["isin"];
    retval.last_price = json["last_price"];
    retval.opening_quantity = json["opening_quantity"];
    retval.pnl = json["pnl"];
    retval.price = json["price"];
    retval.product = json["product"];
    retval.quantity = json["quantity"];
    retval.realised_quantity = json["realised_quantity"];
    retval.t1_quantity = json["t1_quantity"];
    retval.tradingsymbol = json["tradingsymbol"];
    retval.used_quantity = json["used_quantity"];

    return retval;
}

class AuthorisedDate {
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

AuthorisedDate AuthorisedDate_from_JSON(mixed json) {
    AuthorisedDate retval = AuthorisedDate();

    retval.format = json["format"];
    retval.type = json["type"];

    return retval;
}

enum Type {
    BOOLEAN = "boolean", // json: "boolean"
    INTEGER = "integer", // json: "integer"
    NUMBER = "number",   // json: "number"
    STRING = "string",   // json: "string"
}

class AuthorisedQuantity {
    Type type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

AuthorisedQuantity AuthorisedQuantity_from_JSON(mixed json) {
    AuthorisedQuantity retval = AuthorisedQuantity();

    retval.type = json["type"];

    return retval;
}

class HoldingsClass {
    bool               additional_properties; // json: "additionalProperties"
    HoldingsProperties properties;            // json: "properties"
    array(string)      required;              // json: "required"
    string             title;                 // json: "title"
    string             type;                  // json: "type"

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

HoldingsClass HoldingsClass_from_JSON(mixed json) {
    HoldingsClass retval = HoldingsClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class HoldingsProperties {
    Data               data;   // json: "data"
    AuthorisedQuantity status; // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "data" : data,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

HoldingsProperties HoldingsProperties_from_JSON(mixed json) {
    HoldingsProperties retval = HoldingsProperties();

    retval.data = json["data"];
    retval.status = json["status"];

    return retval;
}

class Data {
    Items  items; // json: "items"
    string type;  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "items" : items,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Data Data_from_JSON(mixed json) {
    Data retval = Data();

    retval.items = json["items"];
    retval.type = json["type"];

    return retval;
}

class Items {
    string ref; // json: "$ref"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
        ]);

        return Standards.JSON.encode(json);
    }
}

Items Items_from_JSON(mixed json) {
    Items retval = Items();

    retval.ref = json["$ref"];

    return retval;
}
