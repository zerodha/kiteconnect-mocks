// To parse this JSON data, do
//
//     final orderResponse = orderResponseFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'order_response.freezed.dart';
part 'order_response.g.dart';

@freezed
abstract class OrderResponse with _$OrderResponse {
    const factory OrderResponse({
        Data data,
        String status,
    }) = _OrderResponse;

    factory OrderResponse.fromJson(Map<String, dynamic> json) => _$OrderResponseFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String orderId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
