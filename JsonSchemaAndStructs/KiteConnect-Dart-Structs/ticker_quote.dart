// To parse this JSON data, do
//
//     final tickerQuote = tickerQuoteFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'ticker_quote.freezed.dart';
part 'ticker_quote.g.dart';

@freezed
abstract class TickerQuote with _$TickerQuote {
    const factory TickerQuote({
        double averageTradedPrice,
        double change,
        int instrumentToken,
        int lastPrice,
        int lastTradedQuantity,
        String mode,
        Ohlc ohlc,
        int totalBuyQuantity,
        int totalSellQuantity,
        bool tradable,
        int volumeTraded,
    }) = _TickerQuote;

    factory TickerQuote.fromJson(Map<String, dynamic> json) => _$TickerQuoteFromJson(json);
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
