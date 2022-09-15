// To parse this JSON data, do
//
//     final quote = quoteFromJson(jsonString);

import 'dart:convert';

Quote quoteFromJson(String str) => Quote.fromJson(json.decode(str));

String quoteToJson(Quote data) => json.encode(data.toJson());

class Quote {
    Quote({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Quote.fromJson(Map<String, dynamic> json) => Quote(
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
        this.buy,
        this.data,
        this.depth,
        this.nseInfy,
        this.ohlc,
        this.quote,
    });

    Buy buy;
    Data data;
    Depth depth;
    NseInfyClass nseInfy;
    Ohlc ohlc;
    QuoteClass quote;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        buy: Buy.fromJson(json["Buy"]),
        data: Data.fromJson(json["Data"]),
        depth: Depth.fromJson(json["Depth"]),
        nseInfy: NseInfyClass.fromJson(json["NseInfy"]),
        ohlc: Ohlc.fromJson(json["Ohlc"]),
        quote: QuoteClass.fromJson(json["Quote"]),
    );

    Map<String, dynamic> toJson() => {
        "Buy": buy.toJson(),
        "Data": data.toJson(),
        "Depth": depth.toJson(),
        "NseInfy": nseInfy.toJson(),
        "Ohlc": ohlc.toJson(),
        "Quote": quote.toJson(),
    };
}

class Buy {
    Buy({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    BuyProperties properties;
    List<String> required;
    String title;
    String type;

    factory Buy.fromJson(Map<String, dynamic> json) => Buy(
        additionalProperties: json["additionalProperties"],
        properties: BuyProperties.fromJson(json["properties"]),
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

class BuyProperties {
    BuyProperties({
        this.orders,
        this.price,
        this.quantity,
    });

    Orders orders;
    Orders price;
    Orders quantity;

    factory BuyProperties.fromJson(Map<String, dynamic> json) => BuyProperties(
        orders: Orders.fromJson(json["orders"]),
        price: Orders.fromJson(json["price"]),
        quantity: Orders.fromJson(json["quantity"]),
    );

    Map<String, dynamic> toJson() => {
        "orders": orders.toJson(),
        "price": price.toJson(),
        "quantity": quantity.toJson(),
    };
}

class Orders {
    Orders({
        this.type,
    });

    Type type;

    factory Orders.fromJson(Map<String, dynamic> json) => Orders(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { INTEGER, NUMBER, STRING }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "number": Type.NUMBER,
    "string": Type.STRING
});

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
        this.nseInfy,
    });

    NseInfy nseInfy;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        nseInfy: NseInfy.fromJson(json["NSE:INFY"]),
    );

    Map<String, dynamic> toJson() => {
        "NSE:INFY": nseInfy.toJson(),
    };
}

class NseInfy {
    NseInfy({
        this.ref,
    });

    String ref;

    factory NseInfy.fromJson(Map<String, dynamic> json) => NseInfy(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class Depth {
    Depth({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    DepthProperties properties;
    List<String> required;
    String title;
    String type;

    factory Depth.fromJson(Map<String, dynamic> json) => Depth(
        additionalProperties: json["additionalProperties"],
        properties: DepthProperties.fromJson(json["properties"]),
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

class DepthProperties {
    DepthProperties({
        this.buy,
        this.sell,
    });

    BuyClass buy;
    BuyClass sell;

    factory DepthProperties.fromJson(Map<String, dynamic> json) => DepthProperties(
        buy: BuyClass.fromJson(json["buy"]),
        sell: BuyClass.fromJson(json["sell"]),
    );

    Map<String, dynamic> toJson() => {
        "buy": buy.toJson(),
        "sell": sell.toJson(),
    };
}

class BuyClass {
    BuyClass({
        this.items,
        this.type,
    });

    NseInfy items;
    String type;

    factory BuyClass.fromJson(Map<String, dynamic> json) => BuyClass(
        items: NseInfy.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
    };
}

class NseInfyClass {
    NseInfyClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    NseInfyProperties properties;
    List<String> required;
    String title;
    String type;

    factory NseInfyClass.fromJson(Map<String, dynamic> json) => NseInfyClass(
        additionalProperties: json["additionalProperties"],
        properties: NseInfyProperties.fromJson(json["properties"]),
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

class NseInfyProperties {
    NseInfyProperties({
        this.averagePrice,
        this.buyQuantity,
        this.depth,
        this.instrumentToken,
        this.lastPrice,
        this.lastQuantity,
        this.lastTradeTime,
        this.lowerCircuitLimit,
        this.netChange,
        this.ohlc,
        this.oi,
        this.oiDayHigh,
        this.oiDayLow,
        this.sellQuantity,
        this.timestamp,
        this.upperCircuitLimit,
        this.volume,
    });

    Orders averagePrice;
    Orders buyQuantity;
    NseInfy depth;
    Orders instrumentToken;
    Orders lastPrice;
    Orders lastQuantity;
    LastTradeTime lastTradeTime;
    Orders lowerCircuitLimit;
    Orders netChange;
    NseInfy ohlc;
    Orders oi;
    Orders oiDayHigh;
    Orders oiDayLow;
    Orders sellQuantity;
    LastTradeTime timestamp;
    Orders upperCircuitLimit;
    Orders volume;

    factory NseInfyProperties.fromJson(Map<String, dynamic> json) => NseInfyProperties(
        averagePrice: Orders.fromJson(json["average_price"]),
        buyQuantity: Orders.fromJson(json["buy_quantity"]),
        depth: NseInfy.fromJson(json["depth"]),
        instrumentToken: Orders.fromJson(json["instrument_token"]),
        lastPrice: Orders.fromJson(json["last_price"]),
        lastQuantity: Orders.fromJson(json["last_quantity"]),
        lastTradeTime: LastTradeTime.fromJson(json["last_trade_time"]),
        lowerCircuitLimit: Orders.fromJson(json["lower_circuit_limit"]),
        netChange: Orders.fromJson(json["net_change"]),
        ohlc: NseInfy.fromJson(json["ohlc"]),
        oi: Orders.fromJson(json["oi"]),
        oiDayHigh: Orders.fromJson(json["oi_day_high"]),
        oiDayLow: Orders.fromJson(json["oi_day_low"]),
        sellQuantity: Orders.fromJson(json["sell_quantity"]),
        timestamp: LastTradeTime.fromJson(json["timestamp"]),
        upperCircuitLimit: Orders.fromJson(json["upper_circuit_limit"]),
        volume: Orders.fromJson(json["volume"]),
    );

    Map<String, dynamic> toJson() => {
        "average_price": averagePrice.toJson(),
        "buy_quantity": buyQuantity.toJson(),
        "depth": depth.toJson(),
        "instrument_token": instrumentToken.toJson(),
        "last_price": lastPrice.toJson(),
        "last_quantity": lastQuantity.toJson(),
        "last_trade_time": lastTradeTime.toJson(),
        "lower_circuit_limit": lowerCircuitLimit.toJson(),
        "net_change": netChange.toJson(),
        "ohlc": ohlc.toJson(),
        "oi": oi.toJson(),
        "oi_day_high": oiDayHigh.toJson(),
        "oi_day_low": oiDayLow.toJson(),
        "sell_quantity": sellQuantity.toJson(),
        "timestamp": timestamp.toJson(),
        "upper_circuit_limit": upperCircuitLimit.toJson(),
        "volume": volume.toJson(),
    };
}

class LastTradeTime {
    LastTradeTime({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory LastTradeTime.fromJson(Map<String, dynamic> json) => LastTradeTime(
        format: json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format,
        "type": typeValues.reverse[type],
    };
}

class Ohlc {
    Ohlc({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    OhlcProperties properties;
    List<String> required;
    String title;
    String type;

    factory Ohlc.fromJson(Map<String, dynamic> json) => Ohlc(
        additionalProperties: json["additionalProperties"],
        properties: OhlcProperties.fromJson(json["properties"]),
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

class OhlcProperties {
    OhlcProperties({
        this.close,
        this.high,
        this.low,
        this.open,
    });

    Orders close;
    Orders high;
    Orders low;
    Orders open;

    factory OhlcProperties.fromJson(Map<String, dynamic> json) => OhlcProperties(
        close: Orders.fromJson(json["close"]),
        high: Orders.fromJson(json["high"]),
        low: Orders.fromJson(json["low"]),
        open: Orders.fromJson(json["open"]),
    );

    Map<String, dynamic> toJson() => {
        "close": close.toJson(),
        "high": high.toJson(),
        "low": low.toJson(),
        "open": open.toJson(),
    };
}

class QuoteClass {
    QuoteClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    QuoteProperties properties;
    List<String> required;
    String title;
    String type;

    factory QuoteClass.fromJson(Map<String, dynamic> json) => QuoteClass(
        additionalProperties: json["additionalProperties"],
        properties: QuoteProperties.fromJson(json["properties"]),
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

class QuoteProperties {
    QuoteProperties({
        this.data,
        this.status,
    });

    NseInfy data;
    Orders status;

    factory QuoteProperties.fromJson(Map<String, dynamic> json) => QuoteProperties(
        data: NseInfy.fromJson(json["data"]),
        status: Orders.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
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
