// To parse this JSON data, do
//
//     final mfSipPlace = mfSipPlaceFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_sip_place.freezed.dart';
part 'mf_sip_place.g.dart';

@freezed
abstract class MfSipPlace with _$MfSipPlace {
    const factory MfSipPlace({
        Data data,
        String status,
    }) = _MfSipPlace;

    factory MfSipPlace.fromJson(Map<String, dynamic> json) => _$MfSipPlaceFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String sipId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
