// To parse this JSON data, do
//
//     final marginCommodity = marginCommodityFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'margin_commodity.freezed.dart';
part 'margin_commodity.g.dart';

@freezed
abstract class MarginCommodity with _$MarginCommodity {
    const factory MarginCommodity({
        Data data,
        String status,
    }) = _MarginCommodity;

    factory MarginCommodity.fromJson(Map<String, dynamic> json) => _$MarginCommodityFromJson(json);
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
