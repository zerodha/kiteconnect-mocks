// To parse this JSON data, do
//
//     final orderCancel = orderCancelFromJson(jsonString);

import 'dart:convert';

OrderCancel orderCancelFromJson(String str) => OrderCancel.fromJson(json.decode(str));

String orderCancelToJson(OrderCancel data) => json.encode(data.toJson());

class OrderCancel {
    OrderCancel({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory OrderCancel.fromJson(Map<String, dynamic> json) => OrderCancel(
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
        this.orderCancel,
    });

    Data data;
    OrderCancelClass orderCancel;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        orderCancel: OrderCancelClass.fromJson(json["OrderCancel"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "OrderCancel": orderCancel.toJson(),
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
        this.type,
    });

    String type;

    factory OrderId.fromJson(Map<String, dynamic> json) => OrderId(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
    };
}

class OrderCancelClass {
    OrderCancelClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OrderCancelProperties properties;
    List<String> required;
    String title;
    String type;

    factory OrderCancelClass.fromJson(Map<String, dynamic> json) => OrderCancelClass(
        additionalProperties: json["additionalProperties"],
        properties: OrderCancelProperties.fromJson(json["properties"]),
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

class OrderCancelProperties {
    OrderCancelProperties({
        this.data,
        this.status,
    });

    DataClass data;
    OrderId status;

    factory OrderCancelProperties.fromJson(Map<String, dynamic> json) => OrderCancelProperties(
        data: DataClass.fromJson(json["data"]),
        status: OrderId.fromJson(json["status"]),
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
