// To parse this JSON data, do
//
//     final gttDeleteOrder = gttDeleteOrderFromJson(jsonString);

import 'dart:convert';

GttDeleteOrder gttDeleteOrderFromJson(String str) => GttDeleteOrder.fromJson(json.decode(str));

String gttDeleteOrderToJson(GttDeleteOrder data) => json.encode(data.toJson());

class GttDeleteOrder {
    GttDeleteOrder({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory GttDeleteOrder.fromJson(Map<String, dynamic> json) => GttDeleteOrder(
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
        this.gttDeleteOrder,
    });

    Data data;
    GttDeleteOrderClass gttDeleteOrder;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        gttDeleteOrder: GttDeleteOrderClass.fromJson(json["GttDeleteOrder"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "GttDeleteOrder": gttDeleteOrder.toJson(),
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

class GttDeleteOrderClass {
    GttDeleteOrderClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    GttDeleteOrderProperties properties;
    List<String> required;
    String title;
    String type;

    factory GttDeleteOrderClass.fromJson(Map<String, dynamic> json) => GttDeleteOrderClass(
        additionalProperties: json["additionalProperties"],
        properties: GttDeleteOrderProperties.fromJson(json["properties"]),
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

class GttDeleteOrderProperties {
    GttDeleteOrderProperties({
        this.data,
        this.status,
    });

    DataClass data;
    TriggerId status;

    factory GttDeleteOrderProperties.fromJson(Map<String, dynamic> json) => GttDeleteOrderProperties(
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
