// To parse this JSON data, do
//
//     final mfHoldings = mfHoldingsFromJson(jsonString);

import 'dart:convert';

MfHoldings mfHoldingsFromJson(String str) => MfHoldings.fromJson(json.decode(str));

String mfHoldingsToJson(MfHoldings data) => json.encode(data.toJson());

class MfHoldings {
    MfHoldings({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfHoldings.fromJson(Map<String, dynamic> json) => MfHoldings(
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
        this.mfHoldings,
    });

    Datum datum;
    MfHoldingsClass mfHoldings;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        datum: Datum.fromJson(json["Datum"]),
        mfHoldings: MfHoldingsClass.fromJson(json["MFHoldings"]),
    );

    Map<String, dynamic> toJson() => {
        "Datum": datum.toJson(),
        "MFHoldings": mfHoldings.toJson(),
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
        this.folio,
        this.fund,
        this.lastPrice,
        this.lastPriceDate,
        this.pledgedQuantity,
        this.pnl,
        this.quantity,
        this.tradingsymbol,
    });

    AveragePrice averagePrice;
    AveragePrice folio;
    AveragePrice fund;
    AveragePrice lastPrice;
    AveragePrice lastPriceDate;
    AveragePrice pledgedQuantity;
    AveragePrice pnl;
    AveragePrice quantity;
    AveragePrice tradingsymbol;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        averagePrice: AveragePrice.fromJson(json["average_price"]),
        folio: AveragePrice.fromJson(json["folio"]),
        fund: AveragePrice.fromJson(json["fund"]),
        lastPrice: AveragePrice.fromJson(json["last_price"]),
        lastPriceDate: AveragePrice.fromJson(json["last_price_date"]),
        pledgedQuantity: AveragePrice.fromJson(json["pledged_quantity"]),
        pnl: AveragePrice.fromJson(json["pnl"]),
        quantity: AveragePrice.fromJson(json["quantity"]),
        tradingsymbol: AveragePrice.fromJson(json["tradingsymbol"]),
    );

    Map<String, dynamic> toJson() => {
        "average_price": averagePrice.toJson(),
        "folio": folio.toJson(),
        "fund": fund.toJson(),
        "last_price": lastPrice.toJson(),
        "last_price_date": lastPriceDate.toJson(),
        "pledged_quantity": pledgedQuantity.toJson(),
        "pnl": pnl.toJson(),
        "quantity": quantity.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
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

enum Type { NUMBER, STRING, INTEGER }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class MfHoldingsClass {
    MfHoldingsClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfHoldingsProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfHoldingsClass.fromJson(Map<String, dynamic> json) => MfHoldingsClass(
        additionalProperties: json["additionalProperties"],
        properties: MfHoldingsProperties.fromJson(json["properties"]),
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

class MfHoldingsProperties {
    MfHoldingsProperties({
        this.data,
        this.status,
    });

    Data data;
    AveragePrice status;

    factory MfHoldingsProperties.fromJson(Map<String, dynamic> json) => MfHoldingsProperties(
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
