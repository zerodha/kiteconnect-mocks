// To parse this JSON data, do
//
//     final orderModify = orderModifyFromJson(jsonString);

import 'dart:convert';

OrderModify orderModifyFromJson(String str) => OrderModify.fromJson(json.decode(str));

String orderModifyToJson(OrderModify data) => json.encode(data.toJson());

class OrderModify {
    OrderModify({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory OrderModify.fromJson(Map<String, dynamic> json) => OrderModify(
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
        this.orderModify,
    });

    Data data;
    OrderModifyClass orderModify;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        orderModify: OrderModifyClass.fromJson(json["OrderModify"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "OrderModify": orderModify.toJson(),
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

class OrderModifyClass {
    OrderModifyClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OrderModifyProperties properties;
    List<String> required;
    String title;
    String type;

    factory OrderModifyClass.fromJson(Map<String, dynamic> json) => OrderModifyClass(
        additionalProperties: json["additionalProperties"],
        properties: OrderModifyProperties.fromJson(json["properties"]),
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

class OrderModifyProperties {
    OrderModifyProperties({
        this.data,
        this.status,
    });

    DataClass data;
    OrderId status;

    factory OrderModifyProperties.fromJson(Map<String, dynamic> json) => OrderModifyProperties(
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
