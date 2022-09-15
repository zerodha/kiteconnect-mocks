// To parse this JSON data, do
//
//     final mfOrders = mfOrdersFromJson(jsonString);

import 'dart:convert';

MfOrders mfOrdersFromJson(String str) => MfOrders.fromJson(json.decode(str));

String mfOrdersToJson(MfOrders data) => json.encode(data.toJson());

class MfOrders {
    MfOrders({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfOrders.fromJson(Map<String, dynamic> json) => MfOrders(
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
        this.mfOrders,
    });

    Datum datum;
    MfOrdersClass mfOrders;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        datum: Datum.fromJson(json["Datum"]),
        mfOrders: MfOrdersClass.fromJson(json["MFOrders"]),
    );

    Map<String, dynamic> toJson() => {
        "Datum": datum.toJson(),
        "MFOrders": mfOrders.toJson(),
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
        this.amount,
        this.averagePrice,
        this.exchangeOrderId,
        this.exchangeTimestamp,
        this.folio,
        this.fund,
        this.lastPrice,
        this.lastPriceDate,
        this.orderId,
        this.orderTimestamp,
        this.placedBy,
        this.purchaseType,
        this.quantity,
        this.settlementId,
        this.status,
        this.statusMessage,
        this.tag,
        this.tradingsymbol,
        this.transactionType,
        this.variety,
    });

    Amount amount;
    Amount averagePrice;
    ExchangeOrderId exchangeOrderId;
    ExchangeOrderId exchangeTimestamp;
    Amount folio;
    Amount fund;
    Amount lastPrice;
    LastPriceDate lastPriceDate;
    LastPriceDate orderId;
    LastPriceDate orderTimestamp;
    Amount placedBy;
    Amount purchaseType;
    Amount quantity;
    ExchangeOrderId settlementId;
    Amount status;
    Amount statusMessage;
    Tag tag;
    Amount tradingsymbol;
    Amount transactionType;
    Amount variety;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        amount: Amount.fromJson(json["amount"]),
        averagePrice: Amount.fromJson(json["average_price"]),
        exchangeOrderId: ExchangeOrderId.fromJson(json["exchange_order_id"]),
        exchangeTimestamp: ExchangeOrderId.fromJson(json["exchange_timestamp"]),
        folio: Amount.fromJson(json["folio"]),
        fund: Amount.fromJson(json["fund"]),
        lastPrice: Amount.fromJson(json["last_price"]),
        lastPriceDate: LastPriceDate.fromJson(json["last_price_date"]),
        orderId: LastPriceDate.fromJson(json["order_id"]),
        orderTimestamp: LastPriceDate.fromJson(json["order_timestamp"]),
        placedBy: Amount.fromJson(json["placed_by"]),
        purchaseType: Amount.fromJson(json["purchase_type"]),
        quantity: Amount.fromJson(json["quantity"]),
        settlementId: ExchangeOrderId.fromJson(json["settlement_id"]),
        status: Amount.fromJson(json["status"]),
        statusMessage: Amount.fromJson(json["status_message"]),
        tag: Tag.fromJson(json["tag"]),
        tradingsymbol: Amount.fromJson(json["tradingsymbol"]),
        transactionType: Amount.fromJson(json["transaction_type"]),
        variety: Amount.fromJson(json["variety"]),
    );

    Map<String, dynamic> toJson() => {
        "amount": amount.toJson(),
        "average_price": averagePrice.toJson(),
        "exchange_order_id": exchangeOrderId.toJson(),
        "exchange_timestamp": exchangeTimestamp.toJson(),
        "folio": folio.toJson(),
        "fund": fund.toJson(),
        "last_price": lastPrice.toJson(),
        "last_price_date": lastPriceDate.toJson(),
        "order_id": orderId.toJson(),
        "order_timestamp": orderTimestamp.toJson(),
        "placed_by": placedBy.toJson(),
        "purchase_type": purchaseType.toJson(),
        "quantity": quantity.toJson(),
        "settlement_id": settlementId.toJson(),
        "status": status.toJson(),
        "status_message": statusMessage.toJson(),
        "tag": tag.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "transaction_type": transactionType.toJson(),
        "variety": variety.toJson(),
    };
}

class Amount {
    Amount({
        this.type,
    });

    Type type;

    factory Amount.fromJson(Map<String, dynamic> json) => Amount(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { NUMBER, NULL, STRING }

final typeValues = EnumValues({
    "null": Type.NULL,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class ExchangeOrderId {
    ExchangeOrderId({
        this.anyOf,
    });

    List<LastPriceDate> anyOf;

    factory ExchangeOrderId.fromJson(Map<String, dynamic> json) => ExchangeOrderId(
        anyOf: List<LastPriceDate>.from(json["anyOf"].map((x) => LastPriceDate.fromJson(x))),
    );

    Map<String, dynamic> toJson() => {
        "anyOf": List<dynamic>.from(anyOf.map((x) => x.toJson())),
    };
}

class LastPriceDate {
    LastPriceDate({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory LastPriceDate.fromJson(Map<String, dynamic> json) => LastPriceDate(
        format: json["format"] == null ? null : json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format == null ? null : format,
        "type": typeValues.reverse[type],
    };
}

class Tag {
    Tag({
        this.anyOf,
    });

    List<Amount> anyOf;

    factory Tag.fromJson(Map<String, dynamic> json) => Tag(
        anyOf: List<Amount>.from(json["anyOf"].map((x) => Amount.fromJson(x))),
    );

    Map<String, dynamic> toJson() => {
        "anyOf": List<dynamic>.from(anyOf.map((x) => x.toJson())),
    };
}

class MfOrdersClass {
    MfOrdersClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfOrdersProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfOrdersClass.fromJson(Map<String, dynamic> json) => MfOrdersClass(
        additionalProperties: json["additionalProperties"],
        properties: MfOrdersProperties.fromJson(json["properties"]),
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

class MfOrdersProperties {
    MfOrdersProperties({
        this.data,
        this.status,
    });

    Data data;
    Amount status;

    factory MfOrdersProperties.fromJson(Map<String, dynamic> json) => MfOrdersProperties(
        data: Data.fromJson(json["data"]),
        status: Amount.fromJson(json["status"]),
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
