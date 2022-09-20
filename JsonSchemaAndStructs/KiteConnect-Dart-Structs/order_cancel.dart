// To parse this JSON data, do
//
//     final orderCancel = orderCancelFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'order_cancel.freezed.dart';
part 'order_cancel.g.dart';

@freezed
abstract class OrderCancel with _$OrderCancel {
    const factory OrderCancel({
        Data data,
        String status,
    }) = _OrderCancel;

    factory OrderCancel.fromJson(Map<String, dynamic> json) => _$OrderCancelFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String orderId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
