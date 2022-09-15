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

class OrderModify {
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

OrderModify OrderModify_from_JSON(mixed json) {
    OrderModify retval = OrderModify();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    Data             data;         // json: "Data"
    OrderModifyClass order_modify; // json: "OrderModify"

    string encode_json() {
        mapping(string:mixed) json = ([
            "Data" : data,
            "OrderModify" : order_modify,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.data = json["Data"];
    retval.order_modify = json["OrderModify"];

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
    OrderId order_id; // json: "order_id"

    string encode_json() {
        mapping(string:mixed) json = ([
            "order_id" : order_id,
        ]);

        return Standards.JSON.encode(json);
    }
}

DataProperties DataProperties_from_JSON(mixed json) {
    DataProperties retval = DataProperties();

    retval.order_id = json["order_id"];

    return retval;
}

class OrderId {
    string type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

OrderId OrderId_from_JSON(mixed json) {
    OrderId retval = OrderId();

    retval.type = json["type"];

    return retval;
}

class OrderModifyClass {
    bool                  additional_properties; // json: "additionalProperties"
    OrderModifyProperties properties;            // json: "properties"
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

OrderModifyClass OrderModifyClass_from_JSON(mixed json) {
    OrderModifyClass retval = OrderModifyClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class OrderModifyProperties {
    DataClass data;   // json: "data"
    OrderId   status; // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "data" : data,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

OrderModifyProperties OrderModifyProperties_from_JSON(mixed json) {
    OrderModifyProperties retval = OrderModifyProperties();

    retval.data = json["data"];
    retval.status = json["status"];

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
