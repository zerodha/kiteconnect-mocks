// To parse this JSON data, do
//
//     final holdingsAuth = holdingsAuthFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'holdings_auth.freezed.dart';
part 'holdings_auth.g.dart';

@freezed
abstract class HoldingsAuth with _$HoldingsAuth {
    const factory HoldingsAuth({
        Data data,
        String status,
    }) = _HoldingsAuth;

    factory HoldingsAuth.fromJson(Map<String, dynamic> json) => _$HoldingsAuthFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String requestId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
