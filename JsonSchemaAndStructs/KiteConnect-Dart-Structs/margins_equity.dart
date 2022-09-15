// To parse this JSON data, do
//
//     final marginsEquity = marginsEquityFromJson(jsonString);

import 'dart:convert';

MarginsEquity marginsEquityFromJson(String str) => MarginsEquity.fromJson(json.decode(str));

String marginsEquityToJson(MarginsEquity data) => json.encode(data.toJson());

class MarginsEquity {
    MarginsEquity({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MarginsEquity.fromJson(Map<String, dynamic> json) => MarginsEquity(
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
        this.available,
        this.data,
        this.marginsEquity,
    });

    Available available;
    Data data;
    MarginsEquityClass marginsEquity;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        available: Available.fromJson(json["Available"]),
        data: Data.fromJson(json["Data"]),
        marginsEquity: MarginsEquityClass.fromJson(json["MarginsEquity"]),
    );

    Map<String, dynamic> toJson() => {
        "Available": available.toJson(),
        "Data": data.toJson(),
        "MarginsEquity": marginsEquity.toJson(),
    };
}

class Available {
    Available({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    AvailableProperties properties;
    List<String> required;
    String title;
    String type;

    factory Available.fromJson(Map<String, dynamic> json) => Available(
        additionalProperties: json["additionalProperties"],
        properties: AvailableProperties.fromJson(json["properties"]),
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

class AvailableProperties {
    AvailableProperties({
        this.adhocMargin,
        this.cash,
        this.collateral,
        this.intradayPayin,
        this.liveBalance,
        this.openingBalance,
    });

    AdhocMargin adhocMargin;
    AdhocMargin cash;
    AdhocMargin collateral;
    AdhocMargin intradayPayin;
    AdhocMargin liveBalance;
    AdhocMargin openingBalance;

    factory AvailableProperties.fromJson(Map<String, dynamic> json) => AvailableProperties(
        adhocMargin: AdhocMargin.fromJson(json["adhoc_margin"]),
        cash: AdhocMargin.fromJson(json["cash"]),
        collateral: AdhocMargin.fromJson(json["collateral"]),
        intradayPayin: AdhocMargin.fromJson(json["intraday_payin"]),
        liveBalance: AdhocMargin.fromJson(json["live_balance"]),
        openingBalance: AdhocMargin.fromJson(json["opening_balance"]),
    );

    Map<String, dynamic> toJson() => {
        "adhoc_margin": adhocMargin.toJson(),
        "cash": cash.toJson(),
        "collateral": collateral.toJson(),
        "intraday_payin": intradayPayin.toJson(),
        "live_balance": liveBalance.toJson(),
        "opening_balance": openingBalance.toJson(),
    };
}

class AdhocMargin {
    AdhocMargin({
        this.type,
    });

    String type;

    factory AdhocMargin.fromJson(Map<String, dynamic> json) => AdhocMargin(
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
        this.available,
        this.enabled,
        this.net,
        this.utilised,
    });

    AvailableClass available;
    AdhocMargin enabled;
    AdhocMargin net;
    Utilised utilised;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        available: AvailableClass.fromJson(json["available"]),
        enabled: AdhocMargin.fromJson(json["enabled"]),
        net: AdhocMargin.fromJson(json["net"]),
        utilised: Utilised.fromJson(json["utilised"]),
    );

    Map<String, dynamic> toJson() => {
        "available": available.toJson(),
        "enabled": enabled.toJson(),
        "net": net.toJson(),
        "utilised": utilised.toJson(),
    };
}

class AvailableClass {
    AvailableClass({
        this.ref,
    });

    String ref;

    factory AvailableClass.fromJson(Map<String, dynamic> json) => AvailableClass(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class Utilised {
    Utilised({
        this.additionalProperties,
        this.type,
    });

    AdhocMargin additionalProperties;
    String type;

    factory Utilised.fromJson(Map<String, dynamic> json) => Utilised(
        additionalProperties: AdhocMargin.fromJson(json["additionalProperties"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "additionalProperties": additionalProperties.toJson(),
        "type": type,
    };
}

class MarginsEquityClass {
    MarginsEquityClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MarginsEquityProperties properties;
    List<String> required;
    String title;
    String type;

    factory MarginsEquityClass.fromJson(Map<String, dynamic> json) => MarginsEquityClass(
        additionalProperties: json["additionalProperties"],
        properties: MarginsEquityProperties.fromJson(json["properties"]),
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

class MarginsEquityProperties {
    MarginsEquityProperties({
        this.data,
        this.status,
    });

    AvailableClass data;
    AdhocMargin status;

    factory MarginsEquityProperties.fromJson(Map<String, dynamic> json) => MarginsEquityProperties(
        data: AvailableClass.fromJson(json["data"]),
        status: AdhocMargin.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}
