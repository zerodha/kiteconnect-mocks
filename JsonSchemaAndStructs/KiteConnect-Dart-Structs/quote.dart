// To parse this JSON data, do
//
//     final quote = quoteFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'quote.freezed.dart';
part 'quote.g.dart';

@freezed
abstract class Quote with _$Quote {
    const factory Quote({
        Map<String, Datum> data,
        String status,
    }) = _Quote;

    factory Quote.fromJson(Map<String, dynamic> json) => _$QuoteFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        double averagePrice,
        int buyQuantity,
        Depth depth,
        int instrumentToken,
        double lastPrice,
        int lastQuantity,
        DateTime lastTradeTime,
        double lowerCircuitLimit,
        int netChange,
        Ohlc ohlc,
        int oi,
        int oiDayHigh,
        int oiDayLow,
        int sellQuantity,
        DateTime timestamp,
        double upperCircuitLimit,
        int volume,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}

@freezed
abstract class Depth with _$Depth {
    const factory Depth({
        List<Buy> buy,
        List<Buy> sell,
    }) = _Depth;

    factory Depth.fromJson(Map<String, dynamic> json) => _$DepthFromJson(json);
}

@freezed
abstract class Buy with _$Buy {
    const factory Buy({
        int orders,
        double price,
        int quantity,
    }) = _Buy;

    factory Buy.fromJson(Map<String, dynamic> json) => _$BuyFromJson(json);
}

@freezed
abstract class Ohlc with _$Ohlc {
    const factory Ohlc({
        double close,
        double high,
        double low,
        int open,
    }) = _Ohlc;

    factory Ohlc.fromJson(Map<String, dynamic> json) => _$OhlcFromJson(json);
}
