// To parse this JSON data, do
//
//     final orderInfo = orderInfoFromJson(jsonString);

import 'dart:convert';

OrderInfo orderInfoFromJson(String str) => OrderInfo.fromJson(json.decode(str));

String orderInfoToJson(OrderInfo data) => json.encode(data.toJson());

class OrderInfo {
    OrderInfo({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory OrderInfo.fromJson(Map<String, dynamic> json) => OrderInfo(
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
        this.orderInfo,
    });

    Datum datum;
    OrderInfoClass orderInfo;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        datum: Datum.fromJson(json["Datum"]),
        orderInfo: OrderInfoClass.fromJson(json["OrderInfo"]),
    );

    Map<String, dynamic> toJson() => {
        "Datum": datum.toJson(),
        "OrderInfo": orderInfo.toJson(),
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
        this.cancelledQuantity,
        this.disclosedQuantity,
        this.exchange,
        this.exchangeOrderId,
        this.exchangeTimestamp,
        this.filledQuantity,
        this.instrumentToken,
        this.orderId,
        this.orderTimestamp,
        this.orderType,
        this.parentOrderId,
        this.pendingQuantity,
        this.placedBy,
        this.price,
        this.product,
        this.quantity,
        this.status,
        this.statusMessage,
        this.tag,
        this.tradingsymbol,
        this.transactionType,
        this.triggerPrice,
        this.validity,
        this.variety,
    });

    AveragePrice averagePrice;
    AveragePrice cancelledQuantity;
    AveragePrice disclosedQuantity;
    AveragePrice exchange;
    ExchangeOrderId exchangeOrderId;
    ExchangeTimestamp exchangeTimestamp;
    AveragePrice filledQuantity;
    AveragePrice instrumentToken;
    AveragePrice orderId;
    OrderTimestamp orderTimestamp;
    AveragePrice orderType;
    AveragePrice parentOrderId;
    AveragePrice pendingQuantity;
    AveragePrice placedBy;
    AveragePrice price;
    AveragePrice product;
    AveragePrice quantity;
    AveragePrice status;
    AveragePrice statusMessage;
    AveragePrice tag;
    AveragePrice tradingsymbol;
    AveragePrice transactionType;
    AveragePrice triggerPrice;
    AveragePrice validity;
    AveragePrice variety;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        averagePrice: AveragePrice.fromJson(json["average_price"]),
        cancelledQuantity: AveragePrice.fromJson(json["cancelled_quantity"]),
        disclosedQuantity: AveragePrice.fromJson(json["disclosed_quantity"]),
        exchange: AveragePrice.fromJson(json["exchange"]),
        exchangeOrderId: ExchangeOrderId.fromJson(json["exchange_order_id"]),
        exchangeTimestamp: ExchangeTimestamp.fromJson(json["exchange_timestamp"]),
        filledQuantity: AveragePrice.fromJson(json["filled_quantity"]),
        instrumentToken: AveragePrice.fromJson(json["instrument_token"]),
        orderId: AveragePrice.fromJson(json["order_id"]),
        orderTimestamp: OrderTimestamp.fromJson(json["order_timestamp"]),
        orderType: AveragePrice.fromJson(json["order_type"]),
        parentOrderId: AveragePrice.fromJson(json["parent_order_id"]),
        pendingQuantity: AveragePrice.fromJson(json["pending_quantity"]),
        placedBy: AveragePrice.fromJson(json["placed_by"]),
        price: AveragePrice.fromJson(json["price"]),
        product: AveragePrice.fromJson(json["product"]),
        quantity: AveragePrice.fromJson(json["quantity"]),
        status: AveragePrice.fromJson(json["status"]),
        statusMessage: AveragePrice.fromJson(json["status_message"]),
        tag: AveragePrice.fromJson(json["tag"]),
        tradingsymbol: AveragePrice.fromJson(json["tradingsymbol"]),
        transactionType: AveragePrice.fromJson(json["transaction_type"]),
        triggerPrice: AveragePrice.fromJson(json["trigger_price"]),
        validity: AveragePrice.fromJson(json["validity"]),
        variety: AveragePrice.fromJson(json["variety"]),
    );

    Map<String, dynamic> toJson() => {
        "average_price": averagePrice.toJson(),
        "cancelled_quantity": cancelledQuantity.toJson(),
        "disclosed_quantity": disclosedQuantity.toJson(),
        "exchange": exchange.toJson(),
        "exchange_order_id": exchangeOrderId.toJson(),
        "exchange_timestamp": exchangeTimestamp.toJson(),
        "filled_quantity": filledQuantity.toJson(),
        "instrument_token": instrumentToken.toJson(),
        "order_id": orderId.toJson(),
        "order_timestamp": orderTimestamp.toJson(),
        "order_type": orderType.toJson(),
        "parent_order_id": parentOrderId.toJson(),
        "pending_quantity": pendingQuantity.toJson(),
        "placed_by": placedBy.toJson(),
        "price": price.toJson(),
        "product": product.toJson(),
        "quantity": quantity.toJson(),
        "status": status.toJson(),
        "status_message": statusMessage.toJson(),
        "tag": tag.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "transaction_type": transactionType.toJson(),
        "trigger_price": triggerPrice.toJson(),
        "validity": validity.toJson(),
        "variety": variety.toJson(),
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

enum Type { INTEGER, STRING, NULL, NUMBER }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "null": Type.NULL,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class ExchangeOrderId {
    ExchangeOrderId({
        this.anyOf,
    });

    List<AveragePrice> anyOf;

    factory ExchangeOrderId.fromJson(Map<String, dynamic> json) => ExchangeOrderId(
        anyOf: List<AveragePrice>.from(json["anyOf"].map((x) => AveragePrice.fromJson(x))),
    );

    Map<String, dynamic> toJson() => {
        "anyOf": List<dynamic>.from(anyOf.map((x) => x.toJson())),
    };
}

class ExchangeTimestamp {
    ExchangeTimestamp({
        this.anyOf,
    });

    List<OrderTimestamp> anyOf;

    factory ExchangeTimestamp.fromJson(Map<String, dynamic> json) => ExchangeTimestamp(
        anyOf: List<OrderTimestamp>.from(json["anyOf"].map((x) => OrderTimestamp.fromJson(x))),
    );

    Map<String, dynamic> toJson() => {
        "anyOf": List<dynamic>.from(anyOf.map((x) => x.toJson())),
    };
}

class OrderTimestamp {
    OrderTimestamp({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory OrderTimestamp.fromJson(Map<String, dynamic> json) => OrderTimestamp(
        format: json["format"] == null ? null : json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format == null ? null : format,
        "type": typeValues.reverse[type],
    };
}

class OrderInfoClass {
    OrderInfoClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OrderInfoProperties properties;
    List<String> required;
    String title;
    String type;

    factory OrderInfoClass.fromJson(Map<String, dynamic> json) => OrderInfoClass(
        additionalProperties: json["additionalProperties"],
        properties: OrderInfoProperties.fromJson(json["properties"]),
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

class OrderInfoProperties {
    OrderInfoProperties({
        this.data,
        this.status,
    });

    Data data;
    AveragePrice status;

    factory OrderInfoProperties.fromJson(Map<String, dynamic> json) => OrderInfoProperties(
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
