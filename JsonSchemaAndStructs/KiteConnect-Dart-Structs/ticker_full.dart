// To parse this JSON data, do
//
//     final tickerFull = tickerFullFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'ticker_full.freezed.dart';
part 'ticker_full.g.dart';

@freezed
abstract class TickerFull with _$TickerFull {
    const factory TickerFull({
        double averageTradedPrice,
        double change,
        Depth depth,
        String exchangeTimestamp,
        int instrumentToken,
        int lastPrice,
        String lastTradeTime,
        int lastTradedQuantity,
        String mode,
        Ohlc ohlc,
        int oi,
        int oiDayHigh,
        int oiDayLow,
        int totalBuyQuantity,
        int totalSellQuantity,
        bool tradable,
        int volumeTraded,
    }) = _TickerFull;

    factory TickerFull.fromJson(Map<String, dynamic> json) => _$TickerFullFromJson(json);
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
        int price,
        int quantity,
    }) = _Buy;

    factory Buy.fromJson(Map<String, dynamic> json) => _$BuyFromJson(json);
}

@freezed
abstract class Ohlc with _$Ohlc {
    const factory Ohlc({
        int close,
        int high,
        int low,
        int open,
    }) = _Ohlc;

    factory Ohlc.fromJson(Map<String, dynamic> json) => _$OhlcFromJson(json);
}
