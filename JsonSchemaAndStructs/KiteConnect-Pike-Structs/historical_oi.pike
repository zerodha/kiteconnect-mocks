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

class HistoricalOi {
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

HistoricalOi HistoricalOi_from_JSON(mixed json) {
    HistoricalOi retval = HistoricalOi();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    Candle            candle;        // json: "Candle"
    Data              data;          // json: "Data"
    HistoricalOiClass historical_oi; // json: "HistoricalOi"

    string encode_json() {
        mapping(string:mixed) json = ([
            "Candle" : candle,
            "Data" : data,
            "HistoricalOi" : historical_oi,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.candle = json["Candle"];
    retval.data = json["Data"];
    retval.historical_oi = json["HistoricalOi"];

    return retval;
}

class Candle {
    array(AnyOf) any_of; // json: "anyOf"
    string       title;  // json: "title"

    string encode_json() {
        mapping(string:mixed) json = ([
            "anyOf" : any_of,
            "title" : title,
        ]);

        return Standards.JSON.encode(json);
    }
}

Candle Candle_from_JSON(mixed json) {
    Candle retval = Candle();

    retval.any_of = json["anyOf"];
    retval.title = json["title"];

    return retval;
}

class AnyOf {
    string type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

AnyOf AnyOf_from_JSON(mixed json) {
    AnyOf retval = AnyOf();

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
    Candles candles; // json: "candles"

    string encode_json() {
        mapping(string:mixed) json = ([
            "candles" : candles,
        ]);

        return Standards.JSON.encode(json);
    }
}

DataProperties DataProperties_from_JSON(mixed json) {
    DataProperties retval = DataProperties();

    retval.candles = json["candles"];

    return retval;
}

class Candles {
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

Candles Candles_from_JSON(mixed json) {
    Candles retval = Candles();

    retval.items = json["items"];
    retval.type = json["type"];

    return retval;
}

class Items {
    DataClass items; // json: "items"
    string    type;  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "items" : items,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Items Items_from_JSON(mixed json) {
    Items retval = Items();

    retval.items = json["items"];
    retval.type = json["type"];

    return retval;
}

class DataClass {
    string ref; // json: "$ref"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
        ]);

        return Standards.JSON.encode(json);
    }
}

DataClass DataClass_from_JSON(mixed json) {
    DataClass retval = DataClass();

    retval.ref = json["$ref"];

    return retval;
}

class HistoricalOiClass {
    bool                   additional_properties; // json: "additionalProperties"
    HistoricalOiProperties properties;            // json: "properties"
    array(string)          required;              // json: "required"
    string                 title;                 // json: "title"
    string                 type;                  // json: "type"

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

HistoricalOiClass HistoricalOiClass_from_JSON(mixed json) {
    HistoricalOiClass retval = HistoricalOiClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class HistoricalOiProperties {
    DataClass data;   // json: "data"
    AnyOf     status; // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "data" : data,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

HistoricalOiProperties HistoricalOiProperties_from_JSON(mixed json) {
    HistoricalOiProperties retval = HistoricalOiProperties();

    retval.data = json["data"];
    retval.status = json["status"];

    return retval;
}
