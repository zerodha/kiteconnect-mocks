// To parse this JSON data, do
//
//     final mfOrdersInfo = mfOrdersInfoFromJson(jsonString);

import 'dart:convert';

MfOrdersInfo mfOrdersInfoFromJson(String str) => MfOrdersInfo.fromJson(json.decode(str));

String mfOrdersInfoToJson(MfOrdersInfo data) => json.encode(data.toJson());

class MfOrdersInfo {
    MfOrdersInfo({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfOrdersInfo.fromJson(Map<String, dynamic> json) => MfOrdersInfo(
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
        this.data,
        this.mfOrdersInfo,
    });

    Data data;
    MfOrdersInfoClass mfOrdersInfo;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        mfOrdersInfo: MfOrdersInfoClass.fromJson(json["MFOrdersInfo"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "MFOrdersInfo": mfOrdersInfo.toJson(),
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
    Amount exchangeOrderId;
    Amount exchangeTimestamp;
    Amount folio;
    Amount fund;
    Amount lastPrice;
    LastPriceDate lastPriceDate;
    LastPriceDate orderId;
    LastPriceDate orderTimestamp;
    Amount placedBy;
    Amount purchaseType;
    Amount quantity;
    Amount settlementId;
    Amount status;
    Amount statusMessage;
    Amount tag;
    Amount tradingsymbol;
    Amount transactionType;
    Amount variety;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        amount: Amount.fromJson(json["amount"]),
        averagePrice: Amount.fromJson(json["average_price"]),
        exchangeOrderId: Amount.fromJson(json["exchange_order_id"]),
        exchangeTimestamp: Amount.fromJson(json["exchange_timestamp"]),
        folio: Amount.fromJson(json["folio"]),
        fund: Amount.fromJson(json["fund"]),
        lastPrice: Amount.fromJson(json["last_price"]),
        lastPriceDate: LastPriceDate.fromJson(json["last_price_date"]),
        orderId: LastPriceDate.fromJson(json["order_id"]),
        orderTimestamp: LastPriceDate.fromJson(json["order_timestamp"]),
        placedBy: Amount.fromJson(json["placed_by"]),
        purchaseType: Amount.fromJson(json["purchase_type"]),
        quantity: Amount.fromJson(json["quantity"]),
        settlementId: Amount.fromJson(json["settlement_id"]),
        status: Amount.fromJson(json["status"]),
        statusMessage: Amount.fromJson(json["status_message"]),
        tag: Amount.fromJson(json["tag"]),
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

enum Type { NUMBER, NULL, STRING, INTEGER }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "null": Type.NULL,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class LastPriceDate {
    LastPriceDate({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory LastPriceDate.fromJson(Map<String, dynamic> json) => LastPriceDate(
        format: json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format,
        "type": typeValues.reverse[type],
    };
}

class MfOrdersInfoClass {
    MfOrdersInfoClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfOrdersInfoProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfOrdersInfoClass.fromJson(Map<String, dynamic> json) => MfOrdersInfoClass(
        additionalProperties: json["additionalProperties"],
        properties: MfOrdersInfoProperties.fromJson(json["properties"]),
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

class MfOrdersInfoProperties {
    MfOrdersInfoProperties({
        this.data,
        this.status,
    });

    DataClass data;
    Amount status;

    factory MfOrdersInfoProperties.fromJson(Map<String, dynamic> json) => MfOrdersInfoProperties(
        data: DataClass.fromJson(json["data"]),
        status: Amount.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class DataClass {
    DataClass({
        this.ref,
    });

    String ref;

    factory DataClass.fromJson(Map<String, dynamic> json) => DataClass(
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
