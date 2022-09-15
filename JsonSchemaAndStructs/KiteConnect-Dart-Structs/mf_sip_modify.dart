// To parse this JSON data, do
//
//     final mfSipModify = mfSipModifyFromJson(jsonString);

import 'dart:convert';

MfSipModify mfSipModifyFromJson(String str) => MfSipModify.fromJson(json.decode(str));

String mfSipModifyToJson(MfSipModify data) => json.encode(data.toJson());

class MfSipModify {
    MfSipModify({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfSipModify.fromJson(Map<String, dynamic> json) => MfSipModify(
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
        this.mfsipModify,
    });

    Data data;
    MfsipModifyClass mfsipModify;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        mfsipModify: MfsipModifyClass.fromJson(json["MFSIPModify"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "MFSIPModify": mfsipModify.toJson(),
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

class MfsipModifyClass {
    MfsipModifyClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfsipModifyProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfsipModifyClass.fromJson(Map<String, dynamic> json) => MfsipModifyClass(
        additionalProperties: json["additionalProperties"],
        properties: MfsipModifyProperties.fromJson(json["properties"]),
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

class MfsipModifyProperties {
    MfsipModifyProperties({
        this.data,
        this.status,
    });

    DataClass data;
    Sipid status;

    factory MfsipModifyProperties.fromJson(Map<String, dynamic> json) => MfsipModifyProperties(
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
