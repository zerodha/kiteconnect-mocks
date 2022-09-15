// To parse this JSON data, do
//
//     final postback = postbackFromJson(jsonString);

import 'dart:convert';

Postback postbackFromJson(String str) => Postback.fromJson(json.decode(str));

String postbackToJson(Postback data) => json.encode(data.toJson());

class Postback {
    Postback({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Postback.fromJson(Map<String, dynamic> json) => Postback(
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
        this.meta,
        this.postback,
    });

    Meta meta;
    PostbackClass postback;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        meta: Meta.fromJson(json["Meta"]),
        postback: PostbackClass.fromJson(json["Postback"]),
    );

    Map<String, dynamic> toJson() => {
        "Meta": meta.toJson(),
        "Postback": postback.toJson(),
    };
}

class Meta {
    Meta({
        this.additionalProperties,
        this.title,
        this.type,
    });

    bool additionalProperties;
    String title;
    String type;

    factory Meta.fromJson(Map<String, dynamic> json) => Meta(
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

class PostbackClass {
    PostbackClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    Properties properties;
    List<String> required;
    String title;
    String type;

    factory PostbackClass.fromJson(Map<String, dynamic> json) => PostbackClass(
        additionalProperties: json["additionalProperties"],
        properties: Properties.fromJson(json["properties"]),
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

class Properties {
    Properties({
        this.appId,
        this.averagePrice,
        this.cancelledQuantity,
        this.checksum,
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
        this.tradingsymbol,
        this.transactionType,
        this.triggerPrice,
        this.unfilledQuantity,
        this.userId,
        this.validity,
        this.variety,
    });

    AppId appId;
    AppId averagePrice;
    AppId cancelledQuantity;
    AppId checksum;
    AppId disclosedQuantity;
    AppId exchange;
    AppId exchangeOrderId;
    Timestamp exchangeTimestamp;
    Timestamp exchangeUpdateTimestamp;
    AppId filledQuantity;
    AppId guid;
    AppId instrumentToken;
    AppId marketProtection;
    MetaClass meta;
    AppId orderId;
    Timestamp orderTimestamp;
    AppId orderType;
    AppId parentOrderId;
    AppId pendingQuantity;
    AppId placedBy;
    AppId price;
    AppId product;
    AppId quantity;
    AppId status;
    AppId statusMessage;
    AppId statusMessageRaw;
    AppId tag;
    AppId tradingsymbol;
    AppId transactionType;
    AppId triggerPrice;
    AppId unfilledQuantity;
    AppId userId;
    AppId validity;
    AppId variety;

    factory Properties.fromJson(Map<String, dynamic> json) => Properties(
        appId: AppId.fromJson(json["app_id"]),
        averagePrice: AppId.fromJson(json["average_price"]),
        cancelledQuantity: AppId.fromJson(json["cancelled_quantity"]),
        checksum: AppId.fromJson(json["checksum"]),
        disclosedQuantity: AppId.fromJson(json["disclosed_quantity"]),
        exchange: AppId.fromJson(json["exchange"]),
        exchangeOrderId: AppId.fromJson(json["exchange_order_id"]),
        exchangeTimestamp: Timestamp.fromJson(json["exchange_timestamp"]),
        exchangeUpdateTimestamp: Timestamp.fromJson(json["exchange_update_timestamp"]),
        filledQuantity: AppId.fromJson(json["filled_quantity"]),
        guid: AppId.fromJson(json["guid"]),
        instrumentToken: AppId.fromJson(json["instrument_token"]),
        marketProtection: AppId.fromJson(json["market_protection"]),
        meta: MetaClass.fromJson(json["meta"]),
        orderId: AppId.fromJson(json["order_id"]),
        orderTimestamp: Timestamp.fromJson(json["order_timestamp"]),
        orderType: AppId.fromJson(json["order_type"]),
        parentOrderId: AppId.fromJson(json["parent_order_id"]),
        pendingQuantity: AppId.fromJson(json["pending_quantity"]),
        placedBy: AppId.fromJson(json["placed_by"]),
        price: AppId.fromJson(json["price"]),
        product: AppId.fromJson(json["product"]),
        quantity: AppId.fromJson(json["quantity"]),
        status: AppId.fromJson(json["status"]),
        statusMessage: AppId.fromJson(json["status_message"]),
        statusMessageRaw: AppId.fromJson(json["status_message_raw"]),
        tag: AppId.fromJson(json["tag"]),
        tradingsymbol: AppId.fromJson(json["tradingsymbol"]),
        transactionType: AppId.fromJson(json["transaction_type"]),
        triggerPrice: AppId.fromJson(json["trigger_price"]),
        unfilledQuantity: AppId.fromJson(json["unfilled_quantity"]),
        userId: AppId.fromJson(json["user_id"]),
        validity: AppId.fromJson(json["validity"]),
        variety: AppId.fromJson(json["variety"]),
    );

    Map<String, dynamic> toJson() => {
        "app_id": appId.toJson(),
        "average_price": averagePrice.toJson(),
        "cancelled_quantity": cancelledQuantity.toJson(),
        "checksum": checksum.toJson(),
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
        "tradingsymbol": tradingsymbol.toJson(),
        "transaction_type": transactionType.toJson(),
        "trigger_price": triggerPrice.toJson(),
        "unfilled_quantity": unfilledQuantity.toJson(),
        "user_id": userId.toJson(),
        "validity": validity.toJson(),
        "variety": variety.toJson(),
    };
}

class AppId {
    AppId({
        this.type,
    });

    Type type;

    factory AppId.fromJson(Map<String, dynamic> json) => AppId(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { INTEGER, STRING, NULL }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "null": Type.NULL,
    "string": Type.STRING
});

class Timestamp {
    Timestamp({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory Timestamp.fromJson(Map<String, dynamic> json) => Timestamp(
        format: json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format,
        "type": typeValues.reverse[type],
    };
}

class MetaClass {
    MetaClass({
        this.ref,
    });

    String ref;

    factory MetaClass.fromJson(Map<String, dynamic> json) => MetaClass(
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
