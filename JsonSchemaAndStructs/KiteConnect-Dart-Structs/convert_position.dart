// To parse this JSON data, do
//
//     final convertPosition = convertPositionFromJson(jsonString);

import 'dart:convert';

ConvertPosition convertPositionFromJson(String str) => ConvertPosition.fromJson(json.decode(str));

String convertPositionToJson(ConvertPosition data) => json.encode(data.toJson());

class ConvertPosition {
    ConvertPosition({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory ConvertPosition.fromJson(Map<String, dynamic> json) => ConvertPosition(
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
        this.convertPosition,
    });

    ConvertPositionClass convertPosition;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        convertPosition: ConvertPositionClass.fromJson(json["ConvertPosition"]),
    );

    Map<String, dynamic> toJson() => {
        "ConvertPosition": convertPosition.toJson(),
    };
}

class ConvertPositionClass {
    ConvertPositionClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    Properties properties;
    List<String> required;
    String title;
    String type;

    factory ConvertPositionClass.fromJson(Map<String, dynamic> json) => ConvertPositionClass(
        additionalProperties: json["additionalProperties"],
        properties: Properties.fromJson(json["properties"]),
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

class Properties {
    Properties({
        this.data,
        this.status,
    });

    Data data;
    Data status;

    factory Properties.fromJson(Map<String, dynamic> json) => Properties(
        data: Data.fromJson(json["data"]),
        status: Data.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class Data {
    Data({
        this.type,
    });

    String type;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
    };
}
