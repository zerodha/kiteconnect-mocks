// To parse this JSON data, do
//
//     final gttModifyOrder = gttModifyOrderFromJson(jsonString);

import 'dart:convert';

GttModifyOrder gttModifyOrderFromJson(String str) => GttModifyOrder.fromJson(json.decode(str));

String gttModifyOrderToJson(GttModifyOrder data) => json.encode(data.toJson());

class GttModifyOrder {
    GttModifyOrder({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory GttModifyOrder.fromJson(Map<String, dynamic> json) => GttModifyOrder(
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
        this.gttModifyOrder,
    });

    Data data;
    GttModifyOrderClass gttModifyOrder;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        gttModifyOrder: GttModifyOrderClass.fromJson(json["GttModifyOrder"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "GttModifyOrder": gttModifyOrder.toJson(),
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

class GttModifyOrderClass {
    GttModifyOrderClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    GttModifyOrderProperties properties;
    List<String> required;
    String title;
    String type;

    factory GttModifyOrderClass.fromJson(Map<String, dynamic> json) => GttModifyOrderClass(
        additionalProperties: json["additionalProperties"],
        properties: GttModifyOrderProperties.fromJson(json["properties"]),
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

class GttModifyOrderProperties {
    GttModifyOrderProperties({
        this.data,
        this.status,
    });

    DataClass data;
    TriggerId status;

    factory GttModifyOrderProperties.fromJson(Map<String, dynamic> json) => GttModifyOrderProperties(
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
