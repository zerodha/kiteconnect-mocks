// To parse this JSON data, do
//
//     final orderModify = orderModifyFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'order_modify.freezed.dart';
part 'order_modify.g.dart';

@freezed
abstract class OrderModify with _$OrderModify {
    const factory OrderModify({
        Data data,
        String status,
    }) = _OrderModify;

    factory OrderModify.fromJson(Map<String, dynamic> json) => _$OrderModifyFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String orderId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
