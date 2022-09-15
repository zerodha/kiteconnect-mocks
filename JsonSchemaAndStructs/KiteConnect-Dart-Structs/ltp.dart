// To parse this JSON data, do
//
//     final ltp = ltpFromJson(jsonString);

import 'dart:convert';

Ltp ltpFromJson(String str) => Ltp.fromJson(json.decode(str));

String ltpToJson(Ltp data) => json.encode(data.toJson());

class Ltp {
    Ltp({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Ltp.fromJson(Map<String, dynamic> json) => Ltp(
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
        this.ltp,
        this.nseInfy,
    });

    Data data;
    LtpClass ltp;
    NseInfyClass nseInfy;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        ltp: LtpClass.fromJson(json["Ltp"]),
        nseInfy: NseInfyClass.fromJson(json["NseInfy"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "Ltp": ltp.toJson(),
        "NseInfy": nseInfy.toJson(),
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
    });

    NseInfy nseInfy;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        nseInfy: NseInfy.fromJson(json["NSE:INFY"]),
    );

    Map<String, dynamic> toJson() => {
        "NSE:INFY": nseInfy.toJson(),
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

class LtpClass {
    LtpClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    LtpProperties properties;
    List<String> required;
    String title;
    String type;

    factory LtpClass.fromJson(Map<String, dynamic> json) => LtpClass(
        additionalProperties: json["additionalProperties"],
        properties: LtpProperties.fromJson(json["properties"]),
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

class LtpProperties {
    LtpProperties({
        this.data,
        this.status,
    });

    NseInfy data;
    Status status;

    factory LtpProperties.fromJson(Map<String, dynamic> json) => LtpProperties(
        data: NseInfy.fromJson(json["data"]),
        status: Status.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
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

class NseInfyClass {
    NseInfyClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    NseInfyProperties properties;
    List<String> required;
    String title;
    String type;

    factory NseInfyClass.fromJson(Map<String, dynamic> json) => NseInfyClass(
        additionalProperties: json["additionalProperties"],
        properties: NseInfyProperties.fromJson(json["properties"]),
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

class NseInfyProperties {
    NseInfyProperties({
        this.instrumentToken,
        this.lastPrice,
    });

    Status instrumentToken;
    Status lastPrice;

    factory NseInfyProperties.fromJson(Map<String, dynamic> json) => NseInfyProperties(
        instrumentToken: Status.fromJson(json["instrument_token"]),
        lastPrice: Status.fromJson(json["last_price"]),
    );

    Map<String, dynamic> toJson() => {
        "instrument_token": instrumentToken.toJson(),
        "last_price": lastPrice.toJson(),
    };
}
