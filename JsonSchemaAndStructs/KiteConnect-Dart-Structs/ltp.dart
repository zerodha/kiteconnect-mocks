// To parse this JSON data, do
//
//     final ltp = ltpFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'ltp.freezed.dart';
part 'ltp.g.dart';

@freezed
abstract class Ltp with _$Ltp {
    const factory Ltp({
        Map<String, Datum> data,
        String status,
    }) = _Ltp;

    factory Ltp.fromJson(Map<String, dynamic> json) => _$LtpFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        int instrumentToken,
        double lastPrice,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}
