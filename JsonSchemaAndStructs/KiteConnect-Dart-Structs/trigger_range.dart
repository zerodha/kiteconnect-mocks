// To parse this JSON data, do
//
//     final triggerRange = triggerRangeFromJson(jsonString);

import 'dart:convert';

TriggerRange triggerRangeFromJson(String str) => TriggerRange.fromJson(json.decode(str));

String triggerRangeToJson(TriggerRange data) => json.encode(data.toJson());

class TriggerRange {
    TriggerRange({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory TriggerRange.fromJson(Map<String, dynamic> json) => TriggerRange(
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
        this.nse,
        this.triggerRange,
    });

    Data data;
    Nse nse;
    TriggerRangeClass triggerRange;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        nse: Nse.fromJson(json["Nse"]),
        triggerRange: TriggerRangeClass.fromJson(json["TriggerRange"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "Nse": nse.toJson(),
        "TriggerRange": triggerRange.toJson(),
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
        this.nseInfy,
        this.nseReliance,
    });

    NseInfy nseInfy;
    NseInfy nseReliance;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        nseInfy: NseInfy.fromJson(json["NSE:INFY"]),
        nseReliance: NseInfy.fromJson(json["NSE:RELIANCE"]),
    );

    Map<String, dynamic> toJson() => {
        "NSE:INFY": nseInfy.toJson(),
        "NSE:RELIANCE": nseReliance.toJson(),
    };
}

class NseInfy {
    NseInfy({
        this.ref,
    });

    String ref;

    factory NseInfy.fromJson(Map<String, dynamic> json) => NseInfy(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class Nse {
    Nse({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    NseProperties properties;
    List<String> required;
    String title;
    String type;

    factory Nse.fromJson(Map<String, dynamic> json) => Nse(
        additionalProperties: json["additionalProperties"],
        properties: NseProperties.fromJson(json["properties"]),
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

class NseProperties {
    NseProperties({
        this.instrumentToken,
        this.lower,
        this.upper,
    });

    InstrumentToken instrumentToken;
    InstrumentToken lower;
    InstrumentToken upper;

    factory NseProperties.fromJson(Map<String, dynamic> json) => NseProperties(
        instrumentToken: InstrumentToken.fromJson(json["instrument_token"]),
        lower: InstrumentToken.fromJson(json["lower"]),
        upper: InstrumentToken.fromJson(json["upper"]),
    );

    Map<String, dynamic> toJson() => {
        "instrument_token": instrumentToken.toJson(),
        "lower": lower.toJson(),
        "upper": upper.toJson(),
    };
}

class InstrumentToken {
    InstrumentToken({
        this.type,
    });

    String type;

    factory InstrumentToken.fromJson(Map<String, dynamic> json) => InstrumentToken(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
    };
}

class TriggerRangeClass {
    TriggerRangeClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    TriggerRangeProperties properties;
    List<String> required;
    String title;
    String type;

    factory TriggerRangeClass.fromJson(Map<String, dynamic> json) => TriggerRangeClass(
        additionalProperties: json["additionalProperties"],
        properties: TriggerRangeProperties.fromJson(json["properties"]),
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

class TriggerRangeProperties {
    TriggerRangeProperties({
        this.data,
        this.status,
    });

    NseInfy data;
    InstrumentToken status;

    factory TriggerRangeProperties.fromJson(Map<String, dynamic> json) => TriggerRangeProperties(
        data: NseInfy.fromJson(json["data"]),
        status: InstrumentToken.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}
