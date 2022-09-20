// To parse this JSON data, do
//
//     final marginsEquity = marginsEquityFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'margins_equity.freezed.dart';
part 'margins_equity.g.dart';

@freezed
abstract class MarginsEquity with _$MarginsEquity {
    const factory MarginsEquity({
        Data data,
        String status,
    }) = _MarginsEquity;

    factory MarginsEquity.fromJson(Map<String, dynamic> json) => _$MarginsEquityFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        Available available,
        bool enabled,
        double net,
        Map<String, double> utilised,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}

@freezed
abstract class Available with _$Available {
    const factory Available({
        int adhocMargin,
        double cash,
        int collateral,
        int intradayPayin,
        double liveBalance,
        double openingBalance,
    }) = _Available;

    factory Available.fromJson(Map<String, dynamic> json) => _$AvailableFromJson(json);
}
