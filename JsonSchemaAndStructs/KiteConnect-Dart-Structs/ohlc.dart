// To parse this JSON data, do
//
//     final ohlc = ohlcFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'ohlc.freezed.dart';
part 'ohlc.g.dart';

@freezed
abstract class Ohlc with _$Ohlc {
    const factory Ohlc({
        Map<String, Datum> data,
        String status,
    }) = _Ohlc;

    factory Ohlc.fromJson(Map<String, dynamic> json) => _$OhlcFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        int instrumentToken,
        int lastPrice,
        OhlcClass ohlc,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}

@freezed
abstract class OhlcClass with _$OhlcClass {
    const factory OhlcClass({
        double close,
        double high,
        double low,
        double open,
    }) = _OhlcClass;

    factory OhlcClass.fromJson(Map<String, dynamic> json) => _$OhlcClassFromJson(json);
}
