// To parse this JSON data, do
//
//     final tickerLtp = tickerLtpFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'ticker_ltp.freezed.dart';
part 'ticker_ltp.g.dart';

@freezed
abstract class TickerLtp with _$TickerLtp {
    const factory TickerLtp({
        int instrumentToken,
        int lastPrice,
        String mode,
        bool tradable,
    }) = _TickerLtp;

    factory TickerLtp.fromJson(Map<String, dynamic> json) => _$TickerLtpFromJson(json);
}
