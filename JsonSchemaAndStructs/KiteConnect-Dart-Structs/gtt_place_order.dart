// To parse this JSON data, do
//
//     final gttPlaceOrder = gttPlaceOrderFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'gtt_place_order.freezed.dart';
part 'gtt_place_order.g.dart';

@freezed
abstract class GttPlaceOrder with _$GttPlaceOrder {
    const factory GttPlaceOrder({
        Data data,
        String status,
    }) = _GttPlaceOrder;

    factory GttPlaceOrder.fromJson(Map<String, dynamic> json) => _$GttPlaceOrderFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        int triggerId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
