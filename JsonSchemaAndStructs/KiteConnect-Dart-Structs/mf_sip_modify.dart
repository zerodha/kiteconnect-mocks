// To parse this JSON data, do
//
//     final mfSipModify = mfSipModifyFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_sip_modify.freezed.dart';
part 'mf_sip_modify.g.dart';

@freezed
abstract class MfSipModify with _$MfSipModify {
    const factory MfSipModify({
        Data data,
        String status,
    }) = _MfSipModify;

    factory MfSipModify.fromJson(Map<String, dynamic> json) => _$MfSipModifyFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String sipId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
