// To parse this JSON data, do
//
//     final orderMargins = orderMarginsFromJson(jsonString);

import 'dart:convert';

OrderMargins orderMarginsFromJson(String str) => OrderMargins.fromJson(json.decode(str));

String orderMarginsToJson(OrderMargins data) => json.encode(data.toJson());

class OrderMargins {
    OrderMargins({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory OrderMargins.fromJson(Map<String, dynamic> json) => OrderMargins(
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
        this.datum,
        this.orderMargins,
        this.pnl,
    });

    Datum datum;
    OrderMarginsClass orderMargins;
    PnlClass pnl;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        datum: Datum.fromJson(json["Datum"]),
        orderMargins: OrderMarginsClass.fromJson(json["OrderMargins"]),
        pnl: PnlClass.fromJson(json["Pnl"]),
    );

    Map<String, dynamic> toJson() => {
        "Datum": datum.toJson(),
        "OrderMargins": orderMargins.toJson(),
        "Pnl": pnl.toJson(),
    };
}

class Datum {
    Datum({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    DatumProperties properties;
    List<String> required;
    String title;
    String type;

    factory Datum.fromJson(Map<String, dynamic> json) => Datum(
        additionalProperties: json["additionalProperties"],
        properties: DatumProperties.fromJson(json["properties"]),
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

class DatumProperties {
    DatumProperties({
        this.additional,
        this.bo,
        this.cash,
        this.exchange,
        this.exposure,
        this.optionPremium,
        this.pnl,
        this.span,
        this.total,
        this.tradingsymbol,
        this.type,
        this.datumPropertiesVar,
    });

    Additional additional;
    Additional bo;
    Additional cash;
    Additional exchange;
    Additional exposure;
    Additional optionPremium;
    Pnl pnl;
    Additional span;
    Additional total;
    Additional tradingsymbol;
    Additional type;
    Additional datumPropertiesVar;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        additional: Additional.fromJson(json["additional"]),
        bo: Additional.fromJson(json["bo"]),
        cash: Additional.fromJson(json["cash"]),
        exchange: Additional.fromJson(json["exchange"]),
        exposure: Additional.fromJson(json["exposure"]),
        optionPremium: Additional.fromJson(json["option_premium"]),
        pnl: Pnl.fromJson(json["pnl"]),
        span: Additional.fromJson(json["span"]),
        total: Additional.fromJson(json["total"]),
        tradingsymbol: Additional.fromJson(json["tradingsymbol"]),
        type: Additional.fromJson(json["type"]),
        datumPropertiesVar: Additional.fromJson(json["var"]),
    );

    Map<String, dynamic> toJson() => {
        "additional": additional.toJson(),
        "bo": bo.toJson(),
        "cash": cash.toJson(),
        "exchange": exchange.toJson(),
        "exposure": exposure.toJson(),
        "option_premium": optionPremium.toJson(),
        "pnl": pnl.toJson(),
        "span": span.toJson(),
        "total": total.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "type": type.toJson(),
        "var": datumPropertiesVar.toJson(),
    };
}

class Additional {
    Additional({
        this.type,
    });

    Type type;

    factory Additional.fromJson(Map<String, dynamic> json) => Additional(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { INTEGER, STRING, NUMBER }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class Pnl {
    Pnl({
        this.ref,
    });

    String ref;

    factory Pnl.fromJson(Map<String, dynamic> json) => Pnl(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class OrderMarginsClass {
    OrderMarginsClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OrderMarginsProperties properties;
    List<String> required;
    String title;
    String type;

    factory OrderMarginsClass.fromJson(Map<String, dynamic> json) => OrderMarginsClass(
        additionalProperties: json["additionalProperties"],
        properties: OrderMarginsProperties.fromJson(json["properties"]),
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

class OrderMarginsProperties {
    OrderMarginsProperties({
        this.data,
        this.status,
    });

    Data data;
    Additional status;

    factory OrderMarginsProperties.fromJson(Map<String, dynamic> json) => OrderMarginsProperties(
        data: Data.fromJson(json["data"]),
        status: Additional.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class Data {
    Data({
        this.items,
        this.type,
    });

    Pnl items;
    String type;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
        items: Pnl.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
    };
}

class PnlClass {
    PnlClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    PnlProperties properties;
    List<String> required;
    String title;
    String type;

    factory PnlClass.fromJson(Map<String, dynamic> json) => PnlClass(
        additionalProperties: json["additionalProperties"],
        properties: PnlProperties.fromJson(json["properties"]),
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

class PnlProperties {
    PnlProperties({
        this.realised,
        this.unrealised,
    });

    Additional realised;
    Additional unrealised;

    factory PnlProperties.fromJson(Map<String, dynamic> json) => PnlProperties(
        realised: Additional.fromJson(json["realised"]),
        unrealised: Additional.fromJson(json["unrealised"]),
    );

    Map<String, dynamic> toJson() => {
        "realised": realised.toJson(),
        "unrealised": unrealised.toJson(),
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
