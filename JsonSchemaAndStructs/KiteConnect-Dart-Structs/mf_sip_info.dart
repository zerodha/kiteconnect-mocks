// To parse this JSON data, do
//
//     final mfSipInfo = mfSipInfoFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_sip_info.freezed.dart';
part 'mf_sip_info.g.dart';

@freezed
abstract class MfSipInfo with _$MfSipInfo {
    const factory MfSipInfo({
        Data data,
        String status,
    }) = _MfSipInfo;

    factory MfSipInfo.fromJson(Map<String, dynamic> json) => _$MfSipInfoFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        int completedInstalments,
        DateTime created,
        String dividendType,
        String frequency,
        String fund,
        String fundSource,
        int instalmentAmount,
        int instalmentDay,
        int instalments,
        DateTime lastInstalment,
        DateTime nextInstalment,
        int pendingInstalments,
        String sipId,
        dynamic sipRegNum,
        String sipType,
        String status,
        StepUp stepUp,
        String tag,
        String tradingsymbol,
        String transactionType,
        int triggerPrice,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}

@freezed
abstract class StepUp with _$StepUp {
    const factory StepUp({
        int the1502,
    }) = _StepUp;

    factory StepUp.fromJson(Map<String, dynamic> json) => _$StepUpFromJson(json);
}
