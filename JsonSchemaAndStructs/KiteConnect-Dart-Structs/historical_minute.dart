// To parse this JSON data, do
//
//     final historicalMinute = historicalMinuteFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'historical_minute.freezed.dart';
part 'historical_minute.g.dart';

@freezed
abstract class HistoricalMinute with _$HistoricalMinute {
    const factory HistoricalMinute({
        Data data,
        String status,
    }) = _HistoricalMinute;

    factory HistoricalMinute.fromJson(Map<String, dynamic> json) => _$HistoricalMinuteFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        List<List<dynamic>> candles,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
