// To parse this JSON data, do
//
//     final mfOrderResponse = mfOrderResponseFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_order_response.freezed.dart';
part 'mf_order_response.g.dart';

@freezed
abstract class MfOrderResponse with _$MfOrderResponse {
    const factory MfOrderResponse({
        Data data,
        String status,
    }) = _MfOrderResponse;

    factory MfOrderResponse.fromJson(Map<String, dynamic> json) => _$MfOrderResponseFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String orderId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
