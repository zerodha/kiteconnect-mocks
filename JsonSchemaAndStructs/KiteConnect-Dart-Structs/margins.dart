// To parse this JSON data, do
//
//     final margins = marginsFromJson(jsonString);

import 'dart:convert';

Margins marginsFromJson(String str) => Margins.fromJson(json.decode(str));

String marginsToJson(Margins data) => json.encode(data.toJson());

class Margins {
    Margins({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Margins.fromJson(Map<String, dynamic> json) => Margins(
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
        this.ity,
        this.margins,
    });

    Available available;
    Data data;
    Ity ity;
    MarginsClass margins;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        available: Available.fromJson(json["Available"]),
        data: Data.fromJson(json["Data"]),
        ity: Ity.fromJson(json["Ity"]),
        margins: MarginsClass.fromJson(json["Margins"]),
    );

    Map<String, dynamic> toJson() => {
        "Available": available.toJson(),
        "Data": data.toJson(),
        "Ity": ity.toJson(),
        "Margins": margins.toJson(),
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
        this.commodity,
        this.equity,
    });

    Commodity commodity;
    Commodity equity;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        commodity: Commodity.fromJson(json["commodity"]),
        equity: Commodity.fromJson(json["equity"]),
    );

    Map<String, dynamic> toJson() => {
        "commodity": commodity.toJson(),
        "equity": equity.toJson(),
    };
}

class Commodity {
    Commodity({
        this.ref,
    });

    String ref;

    factory Commodity.fromJson(Map<String, dynamic> json) => Commodity(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class Ity {
    Ity({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    ItyProperties properties;
    List<String> required;
    String title;
    String type;

    factory Ity.fromJson(Map<String, dynamic> json) => Ity(
        additionalProperties: json["additionalProperties"],
        properties: ItyProperties.fromJson(json["properties"]),
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

class ItyProperties {
    ItyProperties({
        this.available,
        this.enabled,
        this.net,
        this.utilised,
    });

    Commodity available;
    AdhocMargin enabled;
    AdhocMargin net;
    Utilised utilised;

    factory ItyProperties.fromJson(Map<String, dynamic> json) => ItyProperties(
        available: Commodity.fromJson(json["available"]),
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

class MarginsClass {
    MarginsClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MarginsProperties properties;
    List<String> required;
    String title;
    String type;

    factory MarginsClass.fromJson(Map<String, dynamic> json) => MarginsClass(
        additionalProperties: json["additionalProperties"],
        properties: MarginsProperties.fromJson(json["properties"]),
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

class MarginsProperties {
    MarginsProperties({
        this.data,
        this.status,
    });

    Commodity data;
    AdhocMargin status;

    factory MarginsProperties.fromJson(Map<String, dynamic> json) => MarginsProperties(
        data: Commodity.fromJson(json["data"]),
        status: AdhocMargin.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}
