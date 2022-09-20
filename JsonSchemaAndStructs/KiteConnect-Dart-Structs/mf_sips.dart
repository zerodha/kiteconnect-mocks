// To parse this JSON data, do
//
//     final mfSips = mfSipsFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_sips.freezed.dart';
part 'mf_sips.g.dart';

@freezed
abstract class MfSips with _$MfSips {
    const factory MfSips({
        List<Datum> data,
    }) = _MfSips;

    factory MfSips.fromJson(Map<String, dynamic> json) => _$MfSipsFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        int completedInstalments,
        DateTime created,
        String dividendType,
        String frequency,
        String fund,
        int instalmentAmount,
        int instalmentDay,
        int instalments,
        DateTime lastInstalment,
        DateTime nextInstalment,
        int pendingInstalments,
        String sipId,
        String sipRegNum,
        String sipType,
        String status,
        Map<String, int> stepUp,
        String tag,
        String tradingsymbol,
        String transactionType,
        int triggerPrice,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}
