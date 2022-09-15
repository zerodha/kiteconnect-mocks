// To parse this JSON data, do
//
//     final orderTrades = orderTradesFromJson(jsonString);

import 'dart:convert';

OrderTrades orderTradesFromJson(String str) => OrderTrades.fromJson(json.decode(str));

String orderTradesToJson(OrderTrades data) => json.encode(data.toJson());

class OrderTrades {
    OrderTrades({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory OrderTrades.fromJson(Map<String, dynamic> json) => OrderTrades(
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
        this.orderTrades,
    });

    Datum datum;
    OrderTradesClass orderTrades;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        datum: Datum.fromJson(json["Datum"]),
        orderTrades: OrderTradesClass.fromJson(json["OrderTrades"]),
    );

    Map<String, dynamic> toJson() => {
        "Datum": datum.toJson(),
        "OrderTrades": orderTrades.toJson(),
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
        this.averagePrice,
        this.exchange,
        this.exchangeOrderId,
        this.exchangeTimestamp,
        this.fillTimestamp,
        this.instrumentToken,
        this.orderId,
        this.orderTimestamp,
        this.product,
        this.quantity,
        this.tradeId,
        this.tradingsymbol,
        this.transactionType,
    });

    AveragePrice averagePrice;
    AveragePrice exchange;
    AveragePrice exchangeOrderId;
    ExchangeTimestamp exchangeTimestamp;
    ExchangeTimestamp fillTimestamp;
    AveragePrice instrumentToken;
    AveragePrice orderId;
    ExchangeTimestamp orderTimestamp;
    AveragePrice product;
    AveragePrice quantity;
    ExchangeTimestamp tradeId;
    AveragePrice tradingsymbol;
    AveragePrice transactionType;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        averagePrice: AveragePrice.fromJson(json["average_price"]),
        exchange: AveragePrice.fromJson(json["exchange"]),
        exchangeOrderId: AveragePrice.fromJson(json["exchange_order_id"]),
        exchangeTimestamp: ExchangeTimestamp.fromJson(json["exchange_timestamp"]),
        fillTimestamp: ExchangeTimestamp.fromJson(json["fill_timestamp"]),
        instrumentToken: AveragePrice.fromJson(json["instrument_token"]),
        orderId: AveragePrice.fromJson(json["order_id"]),
        orderTimestamp: ExchangeTimestamp.fromJson(json["order_timestamp"]),
        product: AveragePrice.fromJson(json["product"]),
        quantity: AveragePrice.fromJson(json["quantity"]),
        tradeId: ExchangeTimestamp.fromJson(json["trade_id"]),
        tradingsymbol: AveragePrice.fromJson(json["tradingsymbol"]),
        transactionType: AveragePrice.fromJson(json["transaction_type"]),
    );

    Map<String, dynamic> toJson() => {
        "average_price": averagePrice.toJson(),
        "exchange": exchange.toJson(),
        "exchange_order_id": exchangeOrderId.toJson(),
        "exchange_timestamp": exchangeTimestamp.toJson(),
        "fill_timestamp": fillTimestamp.toJson(),
        "instrument_token": instrumentToken.toJson(),
        "order_id": orderId.toJson(),
        "order_timestamp": orderTimestamp.toJson(),
        "product": product.toJson(),
        "quantity": quantity.toJson(),
        "trade_id": tradeId.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "transaction_type": transactionType.toJson(),
    };
}

class AveragePrice {
    AveragePrice({
        this.type,
    });

    Type type;

    factory AveragePrice.fromJson(Map<String, dynamic> json) => AveragePrice(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { INTEGER, STRING }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "string": Type.STRING
});

class ExchangeTimestamp {
    ExchangeTimestamp({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory ExchangeTimestamp.fromJson(Map<String, dynamic> json) => ExchangeTimestamp(
        format: json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format,
        "type": typeValues.reverse[type],
    };
}

class OrderTradesClass {
    OrderTradesClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OrderTradesProperties properties;
    List<String> required;
    String title;
    String type;

    factory OrderTradesClass.fromJson(Map<String, dynamic> json) => OrderTradesClass(
        additionalProperties: json["additionalProperties"],
        properties: OrderTradesProperties.fromJson(json["properties"]),
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

class OrderTradesProperties {
    OrderTradesProperties({
        this.data,
        this.status,
    });

    Data data;
    AveragePrice status;

    factory OrderTradesProperties.fromJson(Map<String, dynamic> json) => OrderTradesProperties(
        data: Data.fromJson(json["data"]),
        status: AveragePrice.fromJson(json["status"]),
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

    Items items;
    String type;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
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
        this.ref,
    });

    String ref;

    factory Items.fromJson(Map<String, dynamic> json) => Items(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
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
