// To parse this JSON data, do
//
//     final convertPosition = convertPositionFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'convert_position.freezed.dart';
part 'convert_position.g.dart';

@freezed
abstract class ConvertPosition with _$ConvertPosition {
    const factory ConvertPosition({
        bool data,
        String status,
    }) = _ConvertPosition;

    factory ConvertPosition.fromJson(Map<String, dynamic> json) => _$ConvertPositionFromJson(json);
}
