// To parse this JSON data, do
//
//     final mfSipCancel = mfSipCancelFromJson(jsonString);

import 'dart:convert';

MfSipCancel mfSipCancelFromJson(String str) => MfSipCancel.fromJson(json.decode(str));

String mfSipCancelToJson(MfSipCancel data) => json.encode(data.toJson());

class MfSipCancel {
    MfSipCancel({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfSipCancel.fromJson(Map<String, dynamic> json) => MfSipCancel(
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
        this.mfsipCancel,
    });

    Data data;
    MfsipCancelClass mfsipCancel;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        mfsipCancel: MfsipCancelClass.fromJson(json["MFSIPCancel"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "MFSIPCancel": mfsipCancel.toJson(),
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

class MfsipCancelClass {
    MfsipCancelClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfsipCancelProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfsipCancelClass.fromJson(Map<String, dynamic> json) => MfsipCancelClass(
        additionalProperties: json["additionalProperties"],
        properties: MfsipCancelProperties.fromJson(json["properties"]),
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

class MfsipCancelProperties {
    MfsipCancelProperties({
        this.data,
        this.status,
    });

    DataClass data;
    Sipid status;

    factory MfsipCancelProperties.fromJson(Map<String, dynamic> json) => MfsipCancelProperties(
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
