// To parse this JSON data, do
//
//     final historicalMinute = historicalMinuteFromJson(jsonString);

import 'dart:convert';

HistoricalMinute historicalMinuteFromJson(String str) => HistoricalMinute.fromJson(json.decode(str));

String historicalMinuteToJson(HistoricalMinute data) => json.encode(data.toJson());

class HistoricalMinute {
    HistoricalMinute({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory HistoricalMinute.fromJson(Map<String, dynamic> json) => HistoricalMinute(
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
        this.candle,
        this.data,
        this.historicalMinute,
    });

    Candle candle;
    Data data;
    HistoricalMinuteClass historicalMinute;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        candle: Candle.fromJson(json["Candle"]),
        data: Data.fromJson(json["Data"]),
        historicalMinute: HistoricalMinuteClass.fromJson(json["HistoricalMinute"]),
    );

    Map<String, dynamic> toJson() => {
        "Candle": candle.toJson(),
        "Data": data.toJson(),
        "HistoricalMinute": historicalMinute.toJson(),
    };
}

class Candle {
    Candle({
        this.anyOf,
        this.title,
    });

    List<AnyOf> anyOf;
    String title;

    factory Candle.fromJson(Map<String, dynamic> json) => Candle(
        anyOf: List<AnyOf>.from(json["anyOf"].map((x) => AnyOf.fromJson(x))),
        title: json["title"],
    );

    Map<String, dynamic> toJson() => {
        "anyOf": List<dynamic>.from(anyOf.map((x) => x.toJson())),
        "title": title,
    };
}

class AnyOf {
    AnyOf({
        this.type,
    });

    String type;

    factory AnyOf.fromJson(Map<String, dynamic> json) => AnyOf(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
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
        this.candles,
    });

    Candles candles;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        candles: Candles.fromJson(json["candles"]),
    );

    Map<String, dynamic> toJson() => {
        "candles": candles.toJson(),
    };
}

class Candles {
    Candles({
        this.items,
        this.type,
    });

    Items items;
    String type;

    factory Candles.fromJson(Map<String, dynamic> json) => Candles(
        items: Items.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
    };
}

class Items {
    Items({
        this.items,
        this.type,
    });

    DataClass items;
    String type;

    factory Items.fromJson(Map<String, dynamic> json) => Items(
        items: DataClass.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
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

class HistoricalMinuteClass {
    HistoricalMinuteClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    HistoricalMinuteProperties properties;
    List<String> required;
    String title;
    String type;

    factory HistoricalMinuteClass.fromJson(Map<String, dynamic> json) => HistoricalMinuteClass(
        additionalProperties: json["additionalProperties"],
        properties: HistoricalMinuteProperties.fromJson(json["properties"]),
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

class HistoricalMinuteProperties {
    HistoricalMinuteProperties({
        this.data,
        this.status,
    });

    DataClass data;
    AnyOf status;

    factory HistoricalMinuteProperties.fromJson(Map<String, dynamic> json) => HistoricalMinuteProperties(
        data: DataClass.fromJson(json["data"]),
        status: AnyOf.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}
