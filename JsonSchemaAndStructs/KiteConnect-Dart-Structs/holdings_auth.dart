// To parse this JSON data, do
//
//     final holdingsAuth = holdingsAuthFromJson(jsonString);

import 'dart:convert';

HoldingsAuth holdingsAuthFromJson(String str) => HoldingsAuth.fromJson(json.decode(str));

String holdingsAuthToJson(HoldingsAuth data) => json.encode(data.toJson());

class HoldingsAuth {
    HoldingsAuth({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory HoldingsAuth.fromJson(Map<String, dynamic> json) => HoldingsAuth(
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
        this.holdingsAuth,
    });

    Data data;
    HoldingsAuthClass holdingsAuth;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        holdingsAuth: HoldingsAuthClass.fromJson(json["HoldingsAuth"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "HoldingsAuth": holdingsAuth.toJson(),
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
        this.requestId,
    });

    RequestId requestId;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        requestId: RequestId.fromJson(json["request_id"]),
    );

    Map<String, dynamic> toJson() => {
        "request_id": requestId.toJson(),
    };
}

class RequestId {
    RequestId({
        this.type,
    });

    String type;

    factory RequestId.fromJson(Map<String, dynamic> json) => RequestId(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
    };
}

class HoldingsAuthClass {
    HoldingsAuthClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    HoldingsAuthProperties properties;
    List<String> required;
    String title;
    String type;

    factory HoldingsAuthClass.fromJson(Map<String, dynamic> json) => HoldingsAuthClass(
        additionalProperties: json["additionalProperties"],
        properties: HoldingsAuthProperties.fromJson(json["properties"]),
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

class HoldingsAuthProperties {
    HoldingsAuthProperties({
        this.data,
        this.status,
    });

    DataClass data;
    RequestId status;

    factory HoldingsAuthProperties.fromJson(Map<String, dynamic> json) => HoldingsAuthProperties(
        data: DataClass.fromJson(json["data"]),
        status: RequestId.fromJson(json["status"]),
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
