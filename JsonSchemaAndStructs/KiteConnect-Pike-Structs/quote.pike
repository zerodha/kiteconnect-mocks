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

class Quote {
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

Quote Quote_from_JSON(mixed json) {
    Quote retval = Quote();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    Buy          buy;      // json: "Buy"
    Data         data;     // json: "Data"
    Depth        depth;    // json: "Depth"
    NseInfyClass nse_infy; // json: "NseInfy"
    Ohlc         ohlc;     // json: "Ohlc"
    QuoteClass   quote;    // json: "Quote"

    string encode_json() {
        mapping(string:mixed) json = ([
            "Buy" : buy,
            "Data" : data,
            "Depth" : depth,
            "NseInfy" : nse_infy,
            "Ohlc" : ohlc,
            "Quote" : quote,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.buy = json["Buy"];
    retval.data = json["Data"];
    retval.depth = json["Depth"];
    retval.nse_infy = json["NseInfy"];
    retval.ohlc = json["Ohlc"];
    retval.quote = json["Quote"];

    return retval;
}

class Buy {
    bool          additional_properties; // json: "additionalProperties"
    BuyProperties properties;            // json: "properties"
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

Buy Buy_from_JSON(mixed json) {
    Buy retval = Buy();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class BuyProperties {
    Orders orders;   // json: "orders"
    Orders price;    // json: "price"
    Orders quantity; // json: "quantity"

    string encode_json() {
        mapping(string:mixed) json = ([
            "orders" : orders,
            "price" : price,
            "quantity" : quantity,
        ]);

        return Standards.JSON.encode(json);
    }
}

BuyProperties BuyProperties_from_JSON(mixed json) {
    BuyProperties retval = BuyProperties();

    retval.orders = json["orders"];
    retval.price = json["price"];
    retval.quantity = json["quantity"];

    return retval;
}

class Orders {
    Type type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Orders Orders_from_JSON(mixed json) {
    Orders retval = Orders();

    retval.type = json["type"];

    return retval;
}

enum Type {
    INTEGER = "integer", // json: "integer"
    NUMBER = "number",   // json: "number"
    STRING = "string",   // json: "string"
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
    NseInfy nse_infy; // json: "NSE:INFY"

    string encode_json() {
        mapping(string:mixed) json = ([
            "NSE:INFY" : nse_infy,
        ]);

        return Standards.JSON.encode(json);
    }
}

DataProperties DataProperties_from_JSON(mixed json) {
    DataProperties retval = DataProperties();

    retval.nse_infy = json["NSE:INFY"];

    return retval;
}

class NseInfy {
    string ref; // json: "$ref"

    string encode_json() {
        mapping(string:mixed) json = ([
            "$ref" : ref,
        ]);

        return Standards.JSON.encode(json);
    }
}

NseInfy NseInfy_from_JSON(mixed json) {
    NseInfy retval = NseInfy();

    retval.ref = json["$ref"];

    return retval;
}

class Depth {
    bool            additional_properties; // json: "additionalProperties"
    DepthProperties properties;            // json: "properties"
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

Depth Depth_from_JSON(mixed json) {
    Depth retval = Depth();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class DepthProperties {
    BuyClass buy;  // json: "buy"
    BuyClass sell; // json: "sell"

    string encode_json() {
        mapping(string:mixed) json = ([
            "buy" : buy,
            "sell" : sell,
        ]);

        return Standards.JSON.encode(json);
    }
}

DepthProperties DepthProperties_from_JSON(mixed json) {
    DepthProperties retval = DepthProperties();

    retval.buy = json["buy"];
    retval.sell = json["sell"];

    return retval;
}

class BuyClass {
    NseInfy items; // json: "items"
    string  type;  // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "items" : items,
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

BuyClass BuyClass_from_JSON(mixed json) {
    BuyClass retval = BuyClass();

    retval.items = json["items"];
    retval.type = json["type"];

    return retval;
}

class NseInfyClass {
    bool              additional_properties; // json: "additionalProperties"
    NseInfyProperties properties;            // json: "properties"
    array(string)     required;              // json: "required"
    string            title;                 // json: "title"
    string            type;                  // json: "type"

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

NseInfyClass NseInfyClass_from_JSON(mixed json) {
    NseInfyClass retval = NseInfyClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class NseInfyProperties {
    Orders        average_price;       // json: "average_price"
    Orders        buy_quantity;        // json: "buy_quantity"
    NseInfy       depth;               // json: "depth"
    Orders        instrument_token;    // json: "instrument_token"
    Orders        last_price;          // json: "last_price"
    Orders        last_quantity;       // json: "last_quantity"
    LastTradeTime last_trade_time;     // json: "last_trade_time"
    Orders        lower_circuit_limit; // json: "lower_circuit_limit"
    Orders        net_change;          // json: "net_change"
    NseInfy       ohlc;                // json: "ohlc"
    Orders        oi;                  // json: "oi"
    Orders        oi_day_high;         // json: "oi_day_high"
    Orders        oi_day_low;          // json: "oi_day_low"
    Orders        sell_quantity;       // json: "sell_quantity"
    LastTradeTime timestamp;           // json: "timestamp"
    Orders        upper_circuit_limit; // json: "upper_circuit_limit"
    Orders        volume;              // json: "volume"

    string encode_json() {
        mapping(string:mixed) json = ([
            "average_price" : average_price,
            "buy_quantity" : buy_quantity,
            "depth" : depth,
            "instrument_token" : instrument_token,
            "last_price" : last_price,
            "last_quantity" : last_quantity,
            "last_trade_time" : last_trade_time,
            "lower_circuit_limit" : lower_circuit_limit,
            "net_change" : net_change,
            "ohlc" : ohlc,
            "oi" : oi,
            "oi_day_high" : oi_day_high,
            "oi_day_low" : oi_day_low,
            "sell_quantity" : sell_quantity,
            "timestamp" : timestamp,
            "upper_circuit_limit" : upper_circuit_limit,
            "volume" : volume,
        ]);

        return Standards.JSON.encode(json);
    }
}

NseInfyProperties NseInfyProperties_from_JSON(mixed json) {
    NseInfyProperties retval = NseInfyProperties();

    retval.average_price = json["average_price"];
    retval.buy_quantity = json["buy_quantity"];
    retval.depth = json["depth"];
    retval.instrument_token = json["instrument_token"];
    retval.last_price = json["last_price"];
    retval.last_quantity = json["last_quantity"];
    retval.last_trade_time = json["last_trade_time"];
    retval.lower_circuit_limit = json["lower_circuit_limit"];
    retval.net_change = json["net_change"];
    retval.ohlc = json["ohlc"];
    retval.oi = json["oi"];
    retval.oi_day_high = json["oi_day_high"];
    retval.oi_day_low = json["oi_day_low"];
    retval.sell_quantity = json["sell_quantity"];
    retval.timestamp = json["timestamp"];
    retval.upper_circuit_limit = json["upper_circuit_limit"];
    retval.volume = json["volume"];

    return retval;
}

class LastTradeTime {
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

LastTradeTime LastTradeTime_from_JSON(mixed json) {
    LastTradeTime retval = LastTradeTime();

    retval.format = json["format"];
    retval.type = json["type"];

    return retval;
}

class Ohlc {
    bool           additional_properties; // json: "additionalProperties"
    OhlcProperties properties;            // json: "properties"
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

Ohlc Ohlc_from_JSON(mixed json) {
    Ohlc retval = Ohlc();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class OhlcProperties {
    Orders close; // json: "close"
    Orders high;  // json: "high"
    Orders low;   // json: "low"
    Orders open;  // json: "open"

    string encode_json() {
        mapping(string:mixed) json = ([
            "close" : close,
            "high" : high,
            "low" : low,
            "open" : open,
        ]);

        return Standards.JSON.encode(json);
    }
}

OhlcProperties OhlcProperties_from_JSON(mixed json) {
    OhlcProperties retval = OhlcProperties();

    retval.close = json["close"];
    retval.high = json["high"];
    retval.low = json["low"];
    retval.open = json["open"];

    return retval;
}

class QuoteClass {
    bool            additional_properties; // json: "additionalProperties"
    QuoteProperties properties;            // json: "properties"
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

QuoteClass QuoteClass_from_JSON(mixed json) {
    QuoteClass retval = QuoteClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class QuoteProperties {
    NseInfy data;   // json: "data"
    Orders  status; // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "data" : data,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

QuoteProperties QuoteProperties_from_JSON(mixed json) {
    QuoteProperties retval = QuoteProperties();

    retval.data = json["data"];
    retval.status = json["status"];

    return retval;
}