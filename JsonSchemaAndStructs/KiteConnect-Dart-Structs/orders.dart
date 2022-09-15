// To parse this JSON data, do
//
//     final orders = ordersFromJson(jsonString);

import 'dart:convert';

Orders ordersFromJson(String str) => Orders.fromJson(json.decode(str));

String ordersToJson(Orders data) => json.encode(data.toJson());

class Orders {
    Orders({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Orders.fromJson(Map<String, dynamic> json) => Orders(
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
        this.iceberg,
        this.meta,
        this.orders,
    });

    Datum datum;
    Iceberg iceberg;
    MetaClass meta;
    OrdersClass orders;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        datum: Datum.fromJson(json["Datum"]),
        iceberg: Iceberg.fromJson(json["Iceberg"]),
        meta: MetaClass.fromJson(json["Meta"]),
        orders: OrdersClass.fromJson(json["Orders"]),
    );

    Map<String, dynamic> toJson() => {
        "Datum": datum.toJson(),
        "Iceberg": iceberg.toJson(),
        "Meta": meta.toJson(),
        "Orders": orders.toJson(),
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
        this.exchangeUpdateTimestamp,
        this.filledQuantity,
        this.guid,
        this.instrumentToken,
        this.marketProtection,
        this.meta,
        this.modified,
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
        this.statusMessageRaw,
        this.tag,
        this.tags,
        this.tradingsymbol,
        this.transactionType,
        this.triggerPrice,
        this.validity,
        this.validityTtl,
        this.variety,
    });

    AveragePrice averagePrice;
    AveragePrice cancelledQuantity;
    AveragePrice disclosedQuantity;
    AveragePrice exchange;
    ExchangeOrderId exchangeOrderId;
    ExchangeETimestamp exchangeTimestamp;
    ExchangeETimestamp exchangeUpdateTimestamp;
    AveragePrice filledQuantity;
    AveragePrice guid;
    AveragePrice instrumentToken;
    AveragePrice marketProtection;
    Meta meta;
    AveragePrice modified;
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
    ExchangeOrderId statusMessage;
    ExchangeOrderId statusMessageRaw;
    ExchangeOrderId tag;
    Tags tags;
    AveragePrice tradingsymbol;
    AveragePrice transactionType;
    AveragePrice triggerPrice;
    AveragePrice validity;
    AveragePrice validityTtl;
    AveragePrice variety;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        averagePrice: AveragePrice.fromJson(json["average_price"]),
        cancelledQuantity: AveragePrice.fromJson(json["cancelled_quantity"]),
        disclosedQuantity: AveragePrice.fromJson(json["disclosed_quantity"]),
        exchange: AveragePrice.fromJson(json["exchange"]),
        exchangeOrderId: ExchangeOrderId.fromJson(json["exchange_order_id"]),
        exchangeTimestamp: ExchangeETimestamp.fromJson(json["exchange_timestamp"]),
        exchangeUpdateTimestamp: ExchangeETimestamp.fromJson(json["exchange_update_timestamp"]),
        filledQuantity: AveragePrice.fromJson(json["filled_quantity"]),
        guid: AveragePrice.fromJson(json["guid"]),
        instrumentToken: AveragePrice.fromJson(json["instrument_token"]),
        marketProtection: AveragePrice.fromJson(json["market_protection"]),
        meta: Meta.fromJson(json["meta"]),
        modified: AveragePrice.fromJson(json["modified"]),
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
        statusMessage: ExchangeOrderId.fromJson(json["status_message"]),
        statusMessageRaw: ExchangeOrderId.fromJson(json["status_message_raw"]),
        tag: ExchangeOrderId.fromJson(json["tag"]),
        tags: Tags.fromJson(json["tags"]),
        tradingsymbol: AveragePrice.fromJson(json["tradingsymbol"]),
        transactionType: AveragePrice.fromJson(json["transaction_type"]),
        triggerPrice: AveragePrice.fromJson(json["trigger_price"]),
        validity: AveragePrice.fromJson(json["validity"]),
        validityTtl: AveragePrice.fromJson(json["validity_ttl"]),
        variety: AveragePrice.fromJson(json["variety"]),
    );

    Map<String, dynamic> toJson() => {
        "average_price": averagePrice.toJson(),
        "cancelled_quantity": cancelledQuantity.toJson(),
        "disclosed_quantity": disclosedQuantity.toJson(),
        "exchange": exchange.toJson(),
        "exchange_order_id": exchangeOrderId.toJson(),
        "exchange_timestamp": exchangeTimestamp.toJson(),
        "exchange_update_timestamp": exchangeUpdateTimestamp.toJson(),
        "filled_quantity": filledQuantity.toJson(),
        "guid": guid.toJson(),
        "instrument_token": instrumentToken.toJson(),
        "market_protection": marketProtection.toJson(),
        "meta": meta.toJson(),
        "modified": modified.toJson(),
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
        "status_message_raw": statusMessageRaw.toJson(),
        "tag": tag.toJson(),
        "tags": tags.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "transaction_type": transactionType.toJson(),
        "trigger_price": triggerPrice.toJson(),
        "validity": validity.toJson(),
        "validity_ttl": validityTtl.toJson(),
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

enum Type { INTEGER, STRING, NULL, BOOLEAN }

final typeValues = EnumValues({
    "boolean": Type.BOOLEAN,
    "integer": Type.INTEGER,
    "null": Type.NULL,
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

class ExchangeETimestamp {
    ExchangeETimestamp({
        this.anyOf,
    });

    List<OrderTimestamp> anyOf;

    factory ExchangeETimestamp.fromJson(Map<String, dynamic> json) => ExchangeETimestamp(
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

class Meta {
    Meta({
        this.ref,
    });

    String ref;

    factory Meta.fromJson(Map<String, dynamic> json) => Meta(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class Tags {
    Tags({
        this.items,
        this.type,
    });

    AveragePrice items;
    String type;

    factory Tags.fromJson(Map<String, dynamic> json) => Tags(
        items: AveragePrice.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
    };
}

class Iceberg {
    Iceberg({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    IcebergProperties properties;
    List<String> required;
    String title;
    String type;

    factory Iceberg.fromJson(Map<String, dynamic> json) => Iceberg(
        additionalProperties: json["additionalProperties"],
        properties: IcebergProperties.fromJson(json["properties"]),
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

class IcebergProperties {
    IcebergProperties({
        this.leg,
        this.legQuantity,
        this.legs,
        this.remainingQuantity,
        this.totalQuantity,
    });

    AveragePrice leg;
    AveragePrice legQuantity;
    AveragePrice legs;
    AveragePrice remainingQuantity;
    AveragePrice totalQuantity;

    factory IcebergProperties.fromJson(Map<String, dynamic> json) => IcebergProperties(
        leg: AveragePrice.fromJson(json["leg"]),
        legQuantity: AveragePrice.fromJson(json["leg_quantity"]),
        legs: AveragePrice.fromJson(json["legs"]),
        remainingQuantity: AveragePrice.fromJson(json["remaining_quantity"]),
        totalQuantity: AveragePrice.fromJson(json["total_quantity"]),
    );

    Map<String, dynamic> toJson() => {
        "leg": leg.toJson(),
        "leg_quantity": legQuantity.toJson(),
        "legs": legs.toJson(),
        "remaining_quantity": remainingQuantity.toJson(),
        "total_quantity": totalQuantity.toJson(),
    };
}

class MetaClass {
    MetaClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MetaProperties properties;
    List<dynamic> required;
    String title;
    String type;

    factory MetaClass.fromJson(Map<String, dynamic> json) => MetaClass(
        additionalProperties: json["additionalProperties"],
        properties: MetaProperties.fromJson(json["properties"]),
        required: List<dynamic>.from(json["required"].map((x) => x)),
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

class MetaProperties {
    MetaProperties({
        this.iceberg,
    });

    Meta iceberg;

    factory MetaProperties.fromJson(Map<String, dynamic> json) => MetaProperties(
        iceberg: Meta.fromJson(json["iceberg"]),
    );

    Map<String, dynamic> toJson() => {
        "iceberg": iceberg.toJson(),
    };
}

class OrdersClass {
    OrdersClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OrdersProperties properties;
    List<String> required;
    String title;
    String type;

    factory OrdersClass.fromJson(Map<String, dynamic> json) => OrdersClass(
        additionalProperties: json["additionalProperties"],
        properties: OrdersProperties.fromJson(json["properties"]),
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

class OrdersProperties {
    OrdersProperties({
        this.data,
        this.status,
    });

    Data data;
    AveragePrice status;

    factory OrdersProperties.fromJson(Map<String, dynamic> json) => OrdersProperties(
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

    Meta items;
    String type;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
        items: Meta.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
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
