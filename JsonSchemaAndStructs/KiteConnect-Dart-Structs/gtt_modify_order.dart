// To parse this JSON data, do
//
//     final gttModifyOrder = gttModifyOrderFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'gtt_modify_order.freezed.dart';
part 'gtt_modify_order.g.dart';

@freezed
abstract class GttModifyOrder with _$GttModifyOrder {
    const factory GttModifyOrder({
        Data data,
        String status,
    }) = _GttModifyOrder;

    factory GttModifyOrder.fromJson(Map<String, dynamic> json) => _$GttModifyOrderFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        int triggerId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
