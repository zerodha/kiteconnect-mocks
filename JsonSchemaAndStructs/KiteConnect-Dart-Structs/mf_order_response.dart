// To parse this JSON data, do
//
//     final mfOrderResponse = mfOrderResponseFromJson(jsonString);

import 'dart:convert';

MfOrderResponse mfOrderResponseFromJson(String str) => MfOrderResponse.fromJson(json.decode(str));

String mfOrderResponseToJson(MfOrderResponse data) => json.encode(data.toJson());

class MfOrderResponse {
    MfOrderResponse({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfOrderResponse.fromJson(Map<String, dynamic> json) => MfOrderResponse(
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
        this.mfOrderResponse,
    });

    Data data;
    MfOrderResponseClass mfOrderResponse;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        mfOrderResponse: MfOrderResponseClass.fromJson(json["MFOrderResponse"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "MFOrderResponse": mfOrderResponse.toJson(),
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
        this.orderId,
    });

    OrderId orderId;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        orderId: OrderId.fromJson(json["order_id"]),
    );

    Map<String, dynamic> toJson() => {
        "order_id": orderId.toJson(),
    };
}

class OrderId {
    OrderId({
        this.format,
        this.type,
    });

    String format;
    String type;

    factory OrderId.fromJson(Map<String, dynamic> json) => OrderId(
        format: json["format"],
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "format": format,
        "type": type,
    };
}

class MfOrderResponseClass {
    MfOrderResponseClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfOrderResponseProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfOrderResponseClass.fromJson(Map<String, dynamic> json) => MfOrderResponseClass(
        additionalProperties: json["additionalProperties"],
        properties: MfOrderResponseProperties.fromJson(json["properties"]),
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

class MfOrderResponseProperties {
    MfOrderResponseProperties({
        this.data,
        this.status,
    });

    DataClass data;
    Status status;

    factory MfOrderResponseProperties.fromJson(Map<String, dynamic> json) => MfOrderResponseProperties(
        data: DataClass.fromJson(json["data"]),
        status: Status.fromJson(json["status"]),
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

class Status {
    Status({
        this.type,
    });

    String type;

    factory Status.fromJson(Map<String, dynamic> json) => Status(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
    };
}
