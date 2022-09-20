// To parse this JSON data, do
//
//     final gttDeleteOrder = gttDeleteOrderFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'gtt_delete_order.freezed.dart';
part 'gtt_delete_order.g.dart';

@freezed
abstract class GttDeleteOrder with _$GttDeleteOrder {
    const factory GttDeleteOrder({
        Data data,
        String status,
    }) = _GttDeleteOrder;

    factory GttDeleteOrder.fromJson(Map<String, dynamic> json) => _$GttDeleteOrderFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        int triggerId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
