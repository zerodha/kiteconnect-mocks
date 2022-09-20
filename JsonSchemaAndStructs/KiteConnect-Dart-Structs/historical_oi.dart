// To parse this JSON data, do
//
//     final historicalOi = historicalOiFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'historical_oi.freezed.dart';
part 'historical_oi.g.dart';

@freezed
abstract class HistoricalOi with _$HistoricalOi {
    const factory HistoricalOi({
        Data data,
        String status,
    }) = _HistoricalOi;

    factory HistoricalOi.fromJson(Map<String, dynamic> json) => _$HistoricalOiFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        List<List<dynamic>> candles,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
