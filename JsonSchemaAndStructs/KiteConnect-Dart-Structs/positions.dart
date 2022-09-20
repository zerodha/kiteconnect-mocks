// To parse this JSON data, do
//
//     final positions = positionsFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'positions.freezed.dart';
part 'positions.g.dart';

@freezed
abstract class Positions with _$Positions {
    const factory Positions({
        Data data,
        String status,
    }) = _Positions;

    factory Positions.fromJson(Map<String, dynamic> json) => _$PositionsFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        List<Day> day,
        List<Day> net,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}

@freezed
abstract class Day with _$Day {
    const factory Day({
        double averagePrice,
        int buyM2M,
        double buyPrice,
        int buyQuantity,
        int buyValue,
        int closePrice,
        double dayBuyPrice,
        int dayBuyQuantity,
        int dayBuyValue,
        int daySellPrice,
        int daySellQuantity,
        int daySellValue,
        String exchange,
        int instrumentToken,
        double lastPrice,
        int m2M,
        int multiplier,
        int overnightQuantity,
        int pnl,
        String product,
        int quantity,
        int realised,
        int sellM2M,
        int sellPrice,
        int sellQuantity,
        int sellValue,
        String tradingsymbol,
        int unrealised,
        int value,
    }) = _Day;

    factory Day.fromJson(Map<String, dynamic> json) => _$DayFromJson(json);
}
