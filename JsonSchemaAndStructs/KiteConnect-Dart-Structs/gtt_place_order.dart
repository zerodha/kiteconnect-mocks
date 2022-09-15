// To parse this JSON data, do
//
//     final gttPlaceOrder = gttPlaceOrderFromJson(jsonString);

import 'dart:convert';

GttPlaceOrder gttPlaceOrderFromJson(String str) => GttPlaceOrder.fromJson(json.decode(str));

String gttPlaceOrderToJson(GttPlaceOrder data) => json.encode(data.toJson());

class GttPlaceOrder {
    GttPlaceOrder({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory GttPlaceOrder.fromJson(Map<String, dynamic> json) => GttPlaceOrder(
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
        this.gttPlaceOrder,
    });

    Data data;
    GttPlaceOrderClass gttPlaceOrder;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        gttPlaceOrder: GttPlaceOrderClass.fromJson(json["GttPlaceOrder"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "GttPlaceOrder": gttPlaceOrder.toJson(),
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
        this.triggerId,
    });

    TriggerId triggerId;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        triggerId: TriggerId.fromJson(json["trigger_id"]),
    );

    Map<String, dynamic> toJson() => {
        "trigger_id": triggerId.toJson(),
    };
}

class TriggerId {
    TriggerId({
        this.type,
    });

    String type;

    factory TriggerId.fromJson(Map<String, dynamic> json) => TriggerId(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
    };
}

class GttPlaceOrderClass {
    GttPlaceOrderClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    GttPlaceOrderProperties properties;
    List<String> required;
    String title;
    String type;

    factory GttPlaceOrderClass.fromJson(Map<String, dynamic> json) => GttPlaceOrderClass(
        additionalProperties: json["additionalProperties"],
        properties: GttPlaceOrderProperties.fromJson(json["properties"]),
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

class GttPlaceOrderProperties {
    GttPlaceOrderProperties({
        this.data,
        this.status,
    });

    DataClass data;
    TriggerId status;

    factory GttPlaceOrderProperties.fromJson(Map<String, dynamic> json) => GttPlaceOrderProperties(
        data: DataClass.fromJson(json["data"]),
        status: TriggerId.fromJson(json["status"]),
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
