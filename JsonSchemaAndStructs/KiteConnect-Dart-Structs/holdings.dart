// To parse this JSON data, do
//
//     final holdings = holdingsFromJson(jsonString);

import 'dart:convert';

Holdings holdingsFromJson(String str) => Holdings.fromJson(json.decode(str));

String holdingsToJson(Holdings data) => json.encode(data.toJson());

class Holdings {
    Holdings({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Holdings.fromJson(Map<String, dynamic> json) => Holdings(
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
        this.holdings,
    });

    Datum datum;
    HoldingsClass holdings;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        datum: Datum.fromJson(json["Datum"]),
        holdings: HoldingsClass.fromJson(json["Holdings"]),
    );

    Map<String, dynamic> toJson() => {
        "Datum": datum.toJson(),
        "Holdings": holdings.toJson(),
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
        this.authorisedDate,
        this.authorisedQuantity,
        this.averagePrice,
        this.closePrice,
        this.collateralQuantity,
        this.collateralType,
        this.dayChange,
        this.dayChangePercentage,
        this.discrepancy,
        this.exchange,
        this.instrumentToken,
        this.isin,
        this.lastPrice,
        this.openingQuantity,
        this.pnl,
        this.price,
        this.product,
        this.quantity,
        this.realisedQuantity,
        this.t1Quantity,
        this.tradingsymbol,
        this.usedQuantity,
    });

    AuthorisedDate authorisedDate;
    AuthorisedQuantity authorisedQuantity;
    AuthorisedQuantity averagePrice;
    AuthorisedQuantity closePrice;
    AuthorisedQuantity collateralQuantity;
    AuthorisedQuantity collateralType;
    AuthorisedQuantity dayChange;
    AuthorisedQuantity dayChangePercentage;
    AuthorisedQuantity discrepancy;
    AuthorisedQuantity exchange;
    AuthorisedQuantity instrumentToken;
    AuthorisedQuantity isin;
    AuthorisedQuantity lastPrice;
    AuthorisedQuantity openingQuantity;
    AuthorisedQuantity pnl;
    AuthorisedQuantity price;
    AuthorisedQuantity product;
    AuthorisedQuantity quantity;
    AuthorisedQuantity realisedQuantity;
    AuthorisedQuantity t1Quantity;
    AuthorisedQuantity tradingsymbol;
    AuthorisedQuantity usedQuantity;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        authorisedDate: AuthorisedDate.fromJson(json["authorised_date"]),
        authorisedQuantity: AuthorisedQuantity.fromJson(json["authorised_quantity"]),
        averagePrice: AuthorisedQuantity.fromJson(json["average_price"]),
        closePrice: AuthorisedQuantity.fromJson(json["close_price"]),
        collateralQuantity: AuthorisedQuantity.fromJson(json["collateral_quantity"]),
        collateralType: AuthorisedQuantity.fromJson(json["collateral_type"]),
        dayChange: AuthorisedQuantity.fromJson(json["day_change"]),
        dayChangePercentage: AuthorisedQuantity.fromJson(json["day_change_percentage"]),
        discrepancy: AuthorisedQuantity.fromJson(json["discrepancy"]),
        exchange: AuthorisedQuantity.fromJson(json["exchange"]),
        instrumentToken: AuthorisedQuantity.fromJson(json["instrument_token"]),
        isin: AuthorisedQuantity.fromJson(json["isin"]),
        lastPrice: AuthorisedQuantity.fromJson(json["last_price"]),
        openingQuantity: AuthorisedQuantity.fromJson(json["opening_quantity"]),
        pnl: AuthorisedQuantity.fromJson(json["pnl"]),
        price: AuthorisedQuantity.fromJson(json["price"]),
        product: AuthorisedQuantity.fromJson(json["product"]),
        quantity: AuthorisedQuantity.fromJson(json["quantity"]),
        realisedQuantity: AuthorisedQuantity.fromJson(json["realised_quantity"]),
        t1Quantity: AuthorisedQuantity.fromJson(json["t1_quantity"]),
        tradingsymbol: AuthorisedQuantity.fromJson(json["tradingsymbol"]),
        usedQuantity: AuthorisedQuantity.fromJson(json["used_quantity"]),
    );

    Map<String, dynamic> toJson() => {
        "authorised_date": authorisedDate.toJson(),
        "authorised_quantity": authorisedQuantity.toJson(),
        "average_price": averagePrice.toJson(),
        "close_price": closePrice.toJson(),
        "collateral_quantity": collateralQuantity.toJson(),
        "collateral_type": collateralType.toJson(),
        "day_change": dayChange.toJson(),
        "day_change_percentage": dayChangePercentage.toJson(),
        "discrepancy": discrepancy.toJson(),
        "exchange": exchange.toJson(),
        "instrument_token": instrumentToken.toJson(),
        "isin": isin.toJson(),
        "last_price": lastPrice.toJson(),
        "opening_quantity": openingQuantity.toJson(),
        "pnl": pnl.toJson(),
        "price": price.toJson(),
        "product": product.toJson(),
        "quantity": quantity.toJson(),
        "realised_quantity": realisedQuantity.toJson(),
        "t1_quantity": t1Quantity.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "used_quantity": usedQuantity.toJson(),
    };
}

class AuthorisedDate {
    AuthorisedDate({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory AuthorisedDate.fromJson(Map<String, dynamic> json) => AuthorisedDate(
        format: json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format,
        "type": typeValues.reverse[type],
    };
}

enum Type { INTEGER, NUMBER, STRING, BOOLEAN }

final typeValues = EnumValues({
    "boolean": Type.BOOLEAN,
    "integer": Type.INTEGER,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class AuthorisedQuantity {
    AuthorisedQuantity({
        this.type,
    });

    Type type;

    factory AuthorisedQuantity.fromJson(Map<String, dynamic> json) => AuthorisedQuantity(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

class HoldingsClass {
    HoldingsClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    HoldingsProperties properties;
    List<String> required;
    String title;
    String type;

    factory HoldingsClass.fromJson(Map<String, dynamic> json) => HoldingsClass(
        additionalProperties: json["additionalProperties"],
        properties: HoldingsProperties.fromJson(json["properties"]),
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

class HoldingsProperties {
    HoldingsProperties({
        this.data,
        this.status,
    });

    Data data;
    AuthorisedQuantity status;

    factory HoldingsProperties.fromJson(Map<String, dynamic> json) => HoldingsProperties(
        data: Data.fromJson(json["data"]),
        status: AuthorisedQuantity.fromJson(json["status"]),
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
