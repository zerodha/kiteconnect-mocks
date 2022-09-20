// To parse this JSON data, do
//
//     final mfSipCancel = mfSipCancelFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_sip_cancel.freezed.dart';
part 'mf_sip_cancel.g.dart';

@freezed
abstract class MfSipCancel with _$MfSipCancel {
    const factory MfSipCancel({
        Data data,
        String status,
    }) = _MfSipCancel;

    factory MfSipCancel.fromJson(Map<String, dynamic> json) => _$MfSipCancelFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String sipId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
