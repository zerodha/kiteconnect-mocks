// To parse this JSON data, do
//
//     final gttGetOrders = gttGetOrdersFromJson(jsonString);

import 'dart:convert';

GttGetOrders gttGetOrdersFromJson(String str) => GttGetOrders.fromJson(json.decode(str));

String gttGetOrdersToJson(GttGetOrders data) => json.encode(data.toJson());

class GttGetOrders {
    GttGetOrders({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory GttGetOrders.fromJson(Map<String, dynamic> json) => GttGetOrders(
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
        this.condition,
        this.datum,
        this.gttGetOrders,
        this.meta,
        this.order,
        this.orderResult,
        this.result,
    });

    Condition condition;
    Datum datum;
    GttGetOrdersClass gttGetOrders;
    MetaClass meta;
    Order order;
    OrderResult orderResult;
    Result result;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        condition: Condition.fromJson(json["Condition"]),
        datum: Datum.fromJson(json["Datum"]),
        gttGetOrders: GttGetOrdersClass.fromJson(json["GttGetOrders"]),
        meta: MetaClass.fromJson(json["Meta"]),
        order: Order.fromJson(json["Order"]),
        orderResult: OrderResult.fromJson(json["OrderResult"]),
        result: Result.fromJson(json["Result"]),
    );

    Map<String, dynamic> toJson() => {
        "Condition": condition.toJson(),
        "Datum": datum.toJson(),
        "GttGetOrders": gttGetOrders.toJson(),
        "Meta": meta.toJson(),
        "Order": order.toJson(),
        "OrderResult": orderResult.toJson(),
        "Result": result.toJson(),
    };
}

class Condition {
    Condition({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    ConditionProperties properties;
    List<String> required;
    String title;
    String type;

    factory Condition.fromJson(Map<String, dynamic> json) => Condition(
        additionalProperties: json["additionalProperties"],
        properties: ConditionProperties.fromJson(json["properties"]),
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

class ConditionProperties {
    ConditionProperties({
        this.exchange,
        this.instrumentToken,
        this.lastPrice,
        this.tradingsymbol,
        this.triggerValues,
    });

    Exchange exchange;
    Exchange instrumentToken;
    Exchange lastPrice;
    Exchange tradingsymbol;
    TriggerValues triggerValues;

    factory ConditionProperties.fromJson(Map<String, dynamic> json) => ConditionProperties(
        exchange: Exchange.fromJson(json["exchange"]),
        instrumentToken: Exchange.fromJson(json["instrument_token"]),
        lastPrice: Exchange.fromJson(json["last_price"]),
        tradingsymbol: Exchange.fromJson(json["tradingsymbol"]),
        triggerValues: TriggerValues.fromJson(json["trigger_values"]),
    );

    Map<String, dynamic> toJson() => {
        "exchange": exchange.toJson(),
        "instrument_token": instrumentToken.toJson(),
        "last_price": lastPrice.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "trigger_values": triggerValues.toJson(),
    };
}

class Exchange {
    Exchange({
        this.type,
    });

    Type type;

    factory Exchange.fromJson(Map<String, dynamic> json) => Exchange(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { STRING, INTEGER, NUMBER, NULL }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "null": Type.NULL,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class TriggerValues {
    TriggerValues({
        this.items,
        this.type,
    });

    Exchange items;
    String type;

    factory TriggerValues.fromJson(Map<String, dynamic> json) => TriggerValues(
        items: Exchange.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
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
        this.condition,
        this.createdAt,
        this.expiresAt,
        this.id,
        this.meta,
        this.orders,
        this.parentTrigger,
        this.status,
        this.type,
        this.updatedAt,
        this.userId,
    });

    ConditionClass condition;
    CreatedAt createdAt;
    CreatedAt expiresAt;
    Exchange id;
    Meta meta;
    Orders orders;
    Exchange parentTrigger;
    Exchange status;
    Exchange type;
    CreatedAt updatedAt;
    Exchange userId;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        condition: ConditionClass.fromJson(json["condition"]),
        createdAt: CreatedAt.fromJson(json["created_at"]),
        expiresAt: CreatedAt.fromJson(json["expires_at"]),
        id: Exchange.fromJson(json["id"]),
        meta: Meta.fromJson(json["meta"]),
        orders: Orders.fromJson(json["orders"]),
        parentTrigger: Exchange.fromJson(json["parent_trigger"]),
        status: Exchange.fromJson(json["status"]),
        type: Exchange.fromJson(json["type"]),
        updatedAt: CreatedAt.fromJson(json["updated_at"]),
        userId: Exchange.fromJson(json["user_id"]),
    );

    Map<String, dynamic> toJson() => {
        "condition": condition.toJson(),
        "created_at": createdAt.toJson(),
        "expires_at": expiresAt.toJson(),
        "id": id.toJson(),
        "meta": meta.toJson(),
        "orders": orders.toJson(),
        "parent_trigger": parentTrigger.toJson(),
        "status": status.toJson(),
        "type": type.toJson(),
        "updated_at": updatedAt.toJson(),
        "user_id": userId.toJson(),
    };
}

class ConditionClass {
    ConditionClass({
        this.ref,
    });

    String ref;

    factory ConditionClass.fromJson(Map<String, dynamic> json) => ConditionClass(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class CreatedAt {
    CreatedAt({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory CreatedAt.fromJson(Map<String, dynamic> json) => CreatedAt(
        format: json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format,
        "type": typeValues.reverse[type],
    };
}

class Meta {
    Meta({
        this.anyOf,
    });

    List<AnyOf> anyOf;

    factory Meta.fromJson(Map<String, dynamic> json) => Meta(
        anyOf: List<AnyOf>.from(json["anyOf"].map((x) => AnyOf.fromJson(x))),
    );

    Map<String, dynamic> toJson() => {
        "anyOf": List<dynamic>.from(anyOf.map((x) => x.toJson())),
    };
}

class AnyOf {
    AnyOf({
        this.ref,
        this.type,
    });

    String ref;
    Type type;

    factory AnyOf.fromJson(Map<String, dynamic> json) => AnyOf(
        ref: json["\u0024ref"] == null ? null : json["\u0024ref"],
        type: json["type"] == null ? null : typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref == null ? null : ref,
        "type": type == null ? null : typeValues.reverse[type],
    };
}

class Orders {
    Orders({
        this.items,
        this.type,
    });

    ConditionClass items;
    String type;

    factory Orders.fromJson(Map<String, dynamic> json) => Orders(
        items: ConditionClass.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
    };
}

class GttGetOrdersClass {
    GttGetOrdersClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    GttGetOrdersProperties properties;
    List<String> required;
    String title;
    String type;

    factory GttGetOrdersClass.fromJson(Map<String, dynamic> json) => GttGetOrdersClass(
        additionalProperties: json["additionalProperties"],
        properties: GttGetOrdersProperties.fromJson(json["properties"]),
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

class GttGetOrdersProperties {
    GttGetOrdersProperties({
        this.data,
        this.status,
    });

    Orders data;
    Exchange status;

    factory GttGetOrdersProperties.fromJson(Map<String, dynamic> json) => GttGetOrdersProperties(
        data: Orders.fromJson(json["data"]),
        status: Exchange.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class MetaClass {
    MetaClass({
        this.additionalProperties,
        this.title,
        this.type,
    });

    bool additionalProperties;
    String title;
    String type;

    factory MetaClass.fromJson(Map<String, dynamic> json) => MetaClass(
        additionalProperties: json["additionalProperties"],
        title: json["title"],
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "additionalProperties": additionalProperties,
        "title": title,
        "type": type,
    };
}

class Order {
    Order({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OrderProperties properties;
    List<String> required;
    String title;
    String type;

    factory Order.fromJson(Map<String, dynamic> json) => Order(
        additionalProperties: json["additionalProperties"],
        properties: OrderProperties.fromJson(json["properties"]),
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

class OrderProperties {
    OrderProperties({
        this.exchange,
        this.orderType,
        this.price,
        this.product,
        this.quantity,
        this.result,
        this.tradingsymbol,
        this.transactionType,
    });

    Exchange exchange;
    Exchange orderType;
    Exchange price;
    Exchange product;
    Exchange quantity;
    Meta result;
    Exchange tradingsymbol;
    Exchange transactionType;

    factory OrderProperties.fromJson(Map<String, dynamic> json) => OrderProperties(
        exchange: Exchange.fromJson(json["exchange"]),
        orderType: Exchange.fromJson(json["order_type"]),
        price: Exchange.fromJson(json["price"]),
        product: Exchange.fromJson(json["product"]),
        quantity: Exchange.fromJson(json["quantity"]),
        result: Meta.fromJson(json["result"]),
        tradingsymbol: Exchange.fromJson(json["tradingsymbol"]),
        transactionType: Exchange.fromJson(json["transaction_type"]),
    );

    Map<String, dynamic> toJson() => {
        "exchange": exchange.toJson(),
        "order_type": orderType.toJson(),
        "price": price.toJson(),
        "product": product.toJson(),
        "quantity": quantity.toJson(),
        "result": result.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "transaction_type": transactionType.toJson(),
    };
}

class OrderResult {
    OrderResult({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OrderResultProperties properties;
    List<String> required;
    String title;
    String type;

    factory OrderResult.fromJson(Map<String, dynamic> json) => OrderResult(
        additionalProperties: json["additionalProperties"],
        properties: OrderResultProperties.fromJson(json["properties"]),
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

class OrderResultProperties {
    OrderResultProperties({
        this.orderId,
        this.rejectionReason,
        this.status,
    });

    Exchange orderId;
    Exchange rejectionReason;
    Exchange status;

    factory OrderResultProperties.fromJson(Map<String, dynamic> json) => OrderResultProperties(
        orderId: Exchange.fromJson(json["order_id"]),
        rejectionReason: Exchange.fromJson(json["rejection_reason"]),
        status: Exchange.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "order_id": orderId.toJson(),
        "rejection_reason": rejectionReason.toJson(),
        "status": status.toJson(),
    };
}

class Result {
    Result({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    ResultProperties properties;
    List<String> required;
    String title;
    String type;

    factory Result.fromJson(Map<String, dynamic> json) => Result(
        additionalProperties: json["additionalProperties"],
        properties: ResultProperties.fromJson(json["properties"]),
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

class ResultProperties {
    ResultProperties({
        this.accountId,
        this.exchange,
        this.meta,
        this.orderResult,
        this.orderType,
        this.price,
        this.product,
        this.quantity,
        this.timestamp,
        this.tradingsymbol,
        this.transactionType,
        this.triggeredAt,
        this.validity,
    });

    Exchange accountId;
    Exchange exchange;
    Exchange meta;
    ConditionClass orderResult;
    Exchange orderType;
    Exchange price;
    Exchange product;
    Exchange quantity;
    CreatedAt timestamp;
    Exchange tradingsymbol;
    Exchange transactionType;
    Exchange triggeredAt;
    Exchange validity;

    factory ResultProperties.fromJson(Map<String, dynamic> json) => ResultProperties(
        accountId: Exchange.fromJson(json["account_id"]),
        exchange: Exchange.fromJson(json["exchange"]),
        meta: Exchange.fromJson(json["meta"]),
        orderResult: ConditionClass.fromJson(json["order_result"]),
        orderType: Exchange.fromJson(json["order_type"]),
        price: Exchange.fromJson(json["price"]),
        product: Exchange.fromJson(json["product"]),
        quantity: Exchange.fromJson(json["quantity"]),
        timestamp: CreatedAt.fromJson(json["timestamp"]),
        tradingsymbol: Exchange.fromJson(json["tradingsymbol"]),
        transactionType: Exchange.fromJson(json["transaction_type"]),
        triggeredAt: Exchange.fromJson(json["triggered_at"]),
        validity: Exchange.fromJson(json["validity"]),
    );

    Map<String, dynamic> toJson() => {
        "account_id": accountId.toJson(),
        "exchange": exchange.toJson(),
        "meta": meta.toJson(),
        "order_result": orderResult.toJson(),
        "order_type": orderType.toJson(),
        "price": price.toJson(),
        "product": product.toJson(),
        "quantity": quantity.toJson(),
        "timestamp": timestamp.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "transaction_type": transactionType.toJson(),
        "triggered_at": triggeredAt.toJson(),
        "validity": validity.toJson(),
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
