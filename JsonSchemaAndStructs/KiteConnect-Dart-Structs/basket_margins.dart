// To parse this JSON data, do
//
//     final basketMargins = basketMarginsFromJson(jsonString);

import 'dart:convert';

BasketMargins basketMarginsFromJson(String str) => BasketMargins.fromJson(json.decode(str));

String basketMarginsToJson(BasketMargins data) => json.encode(data.toJson());

class BasketMargins {
    BasketMargins({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory BasketMargins.fromJson(Map<String, dynamic> json) => BasketMargins(
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
        this.basketMargins,
        this.data,
        this.definitionsFinal,
        this.pnl,
    });

    BasketMarginsClass basketMargins;
    DataClass data;
    Final definitionsFinal;
    Pnl pnl;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        basketMargins: BasketMarginsClass.fromJson(json["BasketMargins"]),
        data: DataClass.fromJson(json["Data"]),
        definitionsFinal: Final.fromJson(json["Final"]),
        pnl: Pnl.fromJson(json["Pnl"]),
    );

    Map<String, dynamic> toJson() => {
        "BasketMargins": basketMargins.toJson(),
        "Data": data.toJson(),
        "Final": definitionsFinal.toJson(),
        "Pnl": pnl.toJson(),
    };
}

class BasketMarginsClass {
    BasketMarginsClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    BasketMarginsProperties properties;
    List<String> required;
    String title;
    String type;

    factory BasketMarginsClass.fromJson(Map<String, dynamic> json) => BasketMarginsClass(
        additionalProperties: json["additionalProperties"],
        properties: BasketMarginsProperties.fromJson(json["properties"]),
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

class BasketMarginsProperties {
    BasketMarginsProperties({
        this.data,
        this.status,
    });

    Data data;
    Status status;

    factory BasketMarginsProperties.fromJson(Map<String, dynamic> json) => BasketMarginsProperties(
        data: Data.fromJson(json["data"]),
        status: Status.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class Data {
    Data({
        this.ref,
    });

    String ref;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class Status {
    Status({
        this.type,
    });

    Type type;

    factory Status.fromJson(Map<String, dynamic> json) => Status(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { STRING, INTEGER, NUMBER }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class DataClass {
    DataClass({
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

    factory DataClass.fromJson(Map<String, dynamic> json) => DataClass(
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
        this.dataPropertiesFinal,
        this.initial,
        this.orders,
    });

    Data dataPropertiesFinal;
    Data initial;
    Orders orders;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        dataPropertiesFinal: Data.fromJson(json["final"]),
        initial: Data.fromJson(json["initial"]),
        orders: Orders.fromJson(json["orders"]),
    );

    Map<String, dynamic> toJson() => {
        "final": dataPropertiesFinal.toJson(),
        "initial": initial.toJson(),
        "orders": orders.toJson(),
    };
}

class Orders {
    Orders({
        this.items,
        this.type,
    });

    Data items;
    String type;

    factory Orders.fromJson(Map<String, dynamic> json) => Orders(
        items: Data.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
    };
}

class Final {
    Final({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    FinalProperties properties;
    List<String> required;
    String title;
    String type;

    factory Final.fromJson(Map<String, dynamic> json) => Final(
        additionalProperties: json["additionalProperties"],
        properties: FinalProperties.fromJson(json["properties"]),
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

class FinalProperties {
    FinalProperties({
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
        this.finalPropertiesVar,
    });

    Status additional;
    Status bo;
    Status cash;
    Status exchange;
    Status exposure;
    Status optionPremium;
    Data pnl;
    Status span;
    Status total;
    Status tradingsymbol;
    Status type;
    Status finalPropertiesVar;

    factory FinalProperties.fromJson(Map<String, dynamic> json) => FinalProperties(
        additional: Status.fromJson(json["additional"]),
        bo: Status.fromJson(json["bo"]),
        cash: Status.fromJson(json["cash"]),
        exchange: Status.fromJson(json["exchange"]),
        exposure: Status.fromJson(json["exposure"]),
        optionPremium: Status.fromJson(json["option_premium"]),
        pnl: Data.fromJson(json["pnl"]),
        span: Status.fromJson(json["span"]),
        total: Status.fromJson(json["total"]),
        tradingsymbol: Status.fromJson(json["tradingsymbol"]),
        type: Status.fromJson(json["type"]),
        finalPropertiesVar: Status.fromJson(json["var"]),
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
        "var": finalPropertiesVar.toJson(),
    };
}

class Pnl {
    Pnl({
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

    factory Pnl.fromJson(Map<String, dynamic> json) => Pnl(
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

    Status realised;
    Status unrealised;

    factory PnlProperties.fromJson(Map<String, dynamic> json) => PnlProperties(
        realised: Status.fromJson(json["realised"]),
        unrealised: Status.fromJson(json["unrealised"]),
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
