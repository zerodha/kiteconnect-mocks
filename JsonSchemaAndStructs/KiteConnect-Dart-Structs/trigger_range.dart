// To parse this JSON data, do
//
//     final triggerRange = triggerRangeFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'trigger_range.freezed.dart';
part 'trigger_range.g.dart';

@freezed
abstract class TriggerRange with _$TriggerRange {
    const factory TriggerRange({
        Map<String, Datum> data,
        String status,
    }) = _TriggerRange;

    factory TriggerRange.fromJson(Map<String, dynamic> json) => _$TriggerRangeFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        int instrumentToken,
        double lower,
        double upper,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}
