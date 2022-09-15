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

class MfSipModify {
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

MfSipModify MfSipModify_from_JSON(mixed json) {
    MfSipModify retval = MfSipModify();

    retval.ref = json["$ref"];
    retval.schema = json["$schema"];
    retval.definitions = json["definitions"];

    return retval;
}

class Definitions {
    Data             data;         // json: "Data"
    MfsipModifyClass mfsip_modify; // json: "MFSIPModify"

    string encode_json() {
        mapping(string:mixed) json = ([
            "Data" : data,
            "MFSIPModify" : mfsip_modify,
        ]);

        return Standards.JSON.encode(json);
    }
}

Definitions Definitions_from_JSON(mixed json) {
    Definitions retval = Definitions();

    retval.data = json["Data"];
    retval.mfsip_modify = json["MFSIPModify"];

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
    Sipid sip_id; // json: "sip_id"

    string encode_json() {
        mapping(string:mixed) json = ([
            "sip_id" : sip_id,
        ]);

        return Standards.JSON.encode(json);
    }
}

DataProperties DataProperties_from_JSON(mixed json) {
    DataProperties retval = DataProperties();

    retval.sip_id = json["sip_id"];

    return retval;
}

class Sipid {
    string type; // json: "type"

    string encode_json() {
        mapping(string:mixed) json = ([
            "type" : type,
        ]);

        return Standards.JSON.encode(json);
    }
}

Sipid Sipid_from_JSON(mixed json) {
    Sipid retval = Sipid();

    retval.type = json["type"];

    return retval;
}

class MfsipModifyClass {
    bool                  additional_properties; // json: "additionalProperties"
    MfsipModifyProperties properties;            // json: "properties"
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

MfsipModifyClass MfsipModifyClass_from_JSON(mixed json) {
    MfsipModifyClass retval = MfsipModifyClass();

    retval.additional_properties = json["additionalProperties"];
    retval.properties = json["properties"];
    retval.required = json["required"];
    retval.title = json["title"];
    retval.type = json["type"];

    return retval;
}

class MfsipModifyProperties {
    DataClass data;   // json: "data"
    Sipid     status; // json: "status"

    string encode_json() {
        mapping(string:mixed) json = ([
            "data" : data,
            "status" : status,
        ]);

        return Standards.JSON.encode(json);
    }
}

MfsipModifyProperties MfsipModifyProperties_from_JSON(mixed json) {
    MfsipModifyProperties retval = MfsipModifyProperties();

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
