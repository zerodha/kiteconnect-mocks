// To parse this JSON data, do
//
//     final mfOrderCancel = mfOrderCancelFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_order_cancel.freezed.dart';
part 'mf_order_cancel.g.dart';

@freezed
abstract class MfOrderCancel with _$MfOrderCancel {
    const factory MfOrderCancel({
        Data data,
        String status,
    }) = _MfOrderCancel;

    factory MfOrderCancel.fromJson(Map<String, dynamic> json) => _$MfOrderCancelFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String orderId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
