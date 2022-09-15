// To parse this JSON data, do
//
//     final ohlc = ohlcFromJson(jsonString);

import 'dart:convert';

Ohlc ohlcFromJson(String str) => Ohlc.fromJson(json.decode(str));

String ohlcToJson(Ohlc data) => json.encode(data.toJson());

class Ohlc {
    Ohlc({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Ohlc.fromJson(Map<String, dynamic> json) => Ohlc(
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
        this.nseInfy,
        this.ohlc,
        this.ohlcClass,
    });

    Data data;
    NseInfyClass nseInfy;
    OhlcClass ohlc;
    OhlcClassClass ohlcClass;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        nseInfy: NseInfyClass.fromJson(json["NseInfy"]),
        ohlc: OhlcClass.fromJson(json["Ohlc"]),
        ohlcClass: OhlcClassClass.fromJson(json["OhlcClass"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "NseInfy": nseInfy.toJson(),
        "Ohlc": ohlc.toJson(),
        "OhlcClass": ohlcClass.toJson(),
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
        this.ohlc,
    });

    InstrumentToken instrumentToken;
    InstrumentToken lastPrice;
    NseInfy ohlc;

    factory NseInfyProperties.fromJson(Map<String, dynamic> json) => NseInfyProperties(
        instrumentToken: InstrumentToken.fromJson(json["instrument_token"]),
        lastPrice: InstrumentToken.fromJson(json["last_price"]),
        ohlc: NseInfy.fromJson(json["ohlc"]),
    );

    Map<String, dynamic> toJson() => {
        "instrument_token": instrumentToken.toJson(),
        "last_price": lastPrice.toJson(),
        "ohlc": ohlc.toJson(),
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

class OhlcClass {
    OhlcClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OhlcProperties properties;
    List<String> required;
    String title;
    String type;

    factory OhlcClass.fromJson(Map<String, dynamic> json) => OhlcClass(
        additionalProperties: json["additionalProperties"],
        properties: OhlcProperties.fromJson(json["properties"]),
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

class OhlcProperties {
    OhlcProperties({
        this.data,
        this.status,
    });

    NseInfy data;
    InstrumentToken status;

    factory OhlcProperties.fromJson(Map<String, dynamic> json) => OhlcProperties(
        data: NseInfy.fromJson(json["data"]),
        status: InstrumentToken.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class OhlcClassClass {
    OhlcClassClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OhlcClassProperties properties;
    List<String> required;
    String title;
    String type;

    factory OhlcClassClass.fromJson(Map<String, dynamic> json) => OhlcClassClass(
        additionalProperties: json["additionalProperties"],
        properties: OhlcClassProperties.fromJson(json["properties"]),
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

class OhlcClassProperties {
    OhlcClassProperties({
        this.close,
        this.high,
        this.low,
        this.open,
    });

    InstrumentToken close;
    InstrumentToken high;
    InstrumentToken low;
    InstrumentToken open;

    factory OhlcClassProperties.fromJson(Map<String, dynamic> json) => OhlcClassProperties(
        close: InstrumentToken.fromJson(json["close"]),
        high: InstrumentToken.fromJson(json["high"]),
        low: InstrumentToken.fromJson(json["low"]),
        open: InstrumentToken.fromJson(json["open"]),
    );

    Map<String, dynamic> toJson() => {
        "close": close.toJson(),
        "high": high.toJson(),
        "low": low.toJson(),
        "open": open.toJson(),
    };
}
