// To parse this JSON data, do
//
//     final margins = marginsFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'margins.freezed.dart';
part 'margins.g.dart';

@freezed
abstract class Margins with _$Margins {
    const factory Margins({
        Data data,
        String status,
    }) = _Margins;

    factory Margins.fromJson(Map<String, dynamic> json) => _$MarginsFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        Ity commodity,
        Ity equity,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}

@freezed
abstract class Ity with _$Ity {
    const factory Ity({
        Available available,
        bool enabled,
        double net,
        Map<String, double> utilised,
    }) = _Ity;

    factory Ity.fromJson(Map<String, dynamic> json) => _$ItyFromJson(json);
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
