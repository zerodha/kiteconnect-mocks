// To parse this JSON data, do
//
//     final positions = positionsFromJson(jsonString);

import 'dart:convert';

Positions positionsFromJson(String str) => Positions.fromJson(json.decode(str));

String positionsToJson(Positions data) => json.encode(data.toJson());

class Positions {
    Positions({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Positions.fromJson(Map<String, dynamic> json) => Positions(
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
        this.day,
        this.positions,
    });

    Data data;
    DayClass day;
    PositionsClass positions;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        day: DayClass.fromJson(json["Day"]),
        positions: PositionsClass.fromJson(json["Positions"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "Day": day.toJson(),
        "Positions": positions.toJson(),
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
        this.day,
        this.net,
    });

    Day day;
    Day net;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        day: Day.fromJson(json["day"]),
        net: Day.fromJson(json["net"]),
    );

    Map<String, dynamic> toJson() => {
        "day": day.toJson(),
        "net": net.toJson(),
    };
}

class Day {
    Day({
        this.items,
        this.type,
    });

    DataClass items;
    String type;

    factory Day.fromJson(Map<String, dynamic> json) => Day(
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

class DayClass {
    DayClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    Map<String, Property> properties;
    List<String> required;
    String title;
    String type;

    factory DayClass.fromJson(Map<String, dynamic> json) => DayClass(
        additionalProperties: json["additionalProperties"],
        properties: Map.from(json["properties"]).map((k, v) => MapEntry<String, Property>(k, Property.fromJson(v))),
        required: List<String>.from(json["required"].map((x) => x)),
        title: json["title"],
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "additionalProperties": additionalProperties,
        "properties": Map.from(properties).map((k, v) => MapEntry<String, dynamic>(k, v.toJson())),
        "required": List<dynamic>.from(required.map((x) => x)),
        "title": title,
        "type": type,
    };
}

class Property {
    Property({
        this.type,
    });

    Type type;

    factory Property.fromJson(Map<String, dynamic> json) => Property(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { NUMBER, INTEGER, STRING }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class PositionsClass {
    PositionsClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    PositionsProperties properties;
    List<String> required;
    String title;
    String type;

    factory PositionsClass.fromJson(Map<String, dynamic> json) => PositionsClass(
        additionalProperties: json["additionalProperties"],
        properties: PositionsProperties.fromJson(json["properties"]),
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

class PositionsProperties {
    PositionsProperties({
        this.data,
        this.status,
    });

    DataClass data;
    Property status;

    factory PositionsProperties.fromJson(Map<String, dynamic> json) => PositionsProperties(
        data: DataClass.fromJson(json["data"]),
        status: Property.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class EnumValues<T> {
    Map<String, T> map;
    Map<T, String> reverseMap;

    EnumValues(this.map);

    Map<T, String> get reverse {
        if (reverseMap == null) {
            reverseMap = map.map((k, v) => new MapEntry(v, k));
        }
        return reverseMap;
    }
}
