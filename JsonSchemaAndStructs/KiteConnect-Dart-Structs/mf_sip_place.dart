// To parse this JSON data, do
//
//     final mfSipPlace = mfSipPlaceFromJson(jsonString);

import 'dart:convert';

MfSipPlace mfSipPlaceFromJson(String str) => MfSipPlace.fromJson(json.decode(str));

String mfSipPlaceToJson(MfSipPlace data) => json.encode(data.toJson());

class MfSipPlace {
    MfSipPlace({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfSipPlace.fromJson(Map<String, dynamic> json) => MfSipPlace(
        ref: json["\u0024ref"],
        schema: json["\u0024schema"],
        definitions: Definitions.fromJson(json["definitions"]),
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
        "\u0024schema": schema,
        "definitions": definitions.toJson(),
    };
}

class Definitions {
    Definitions({
        this.data,
        this.mfsipPlace,
    });

    Data data;
    MfsipPlaceClass mfsipPlace;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        mfsipPlace: MfsipPlaceClass.fromJson(json["MFSIPPlace"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "MFSIPPlace": mfsipPlace.toJson(),
    };
}

class Data {
    Data({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    DataProperties properties;
    List<String> required;
    String title;
    String type;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
        additionalProperties: json["additionalProperties"],
        properties: DataProperties.fromJson(json["properties"]),
        required: List<String>.from(json["required"].map((x) => x)),
        title: json["title"],
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "additionalProperties": additionalProperties,
        "properties": properties.toJson(),
        "required": List<dynamic>.from(required.map((x) => x)),
        "title": title,
        "type": type,
    };
}

class DataProperties {
    DataProperties({
        this.sipId,
    });

    Sipid sipId;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        sipId: Sipid.fromJson(json["sip_id"]),
    );

    Map<String, dynamic> toJson() => {
        "sip_id": sipId.toJson(),
    };
}

class Sipid {
    Sipid({
        this.type,
    });

    String type;

    factory Sipid.fromJson(Map<String, dynamic> json) => Sipid(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
    };
}

class MfsipPlaceClass {
    MfsipPlaceClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfsipPlaceProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfsipPlaceClass.fromJson(Map<String, dynamic> json) => MfsipPlaceClass(
        additionalProperties: json["additionalProperties"],
        properties: MfsipPlaceProperties.fromJson(json["properties"]),
        required: List<String>.from(json["required"].map((x) => x)),
        title: json["title"],
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "additionalProperties": additionalProperties,
        "properties": properties.toJson(),
        "required": List<dynamic>.from(required.map((x) => x)),
        "title": title,
        "type": type,
    };
}

class MfsipPlaceProperties {
    MfsipPlaceProperties({
        this.data,
        this.status,
    });

    DataClass data;
    Sipid status;

    factory MfsipPlaceProperties.fromJson(Map<String, dynamic> json) => MfsipPlaceProperties(
        data: DataClass.fromJson(json["data"]),
        status: Sipid.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class DataClass {
    DataClass({
        this.ref,
    });

    String ref;

    factory DataClass.fromJson(Map<String, dynamic> json) => DataClass(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}
