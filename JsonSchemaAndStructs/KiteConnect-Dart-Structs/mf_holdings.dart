// To parse this JSON data, do
//
//     final mfHoldings = mfHoldingsFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_holdings.freezed.dart';
part 'mf_holdings.g.dart';

@freezed
abstract class MfHoldings with _$MfHoldings {
    const factory MfHoldings({
        List<Datum> data,
        String status,
    }) = _MfHoldings;

    factory MfHoldings.fromJson(Map<String, dynamic> json) => _$MfHoldingsFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        double averagePrice,
        String folio,
        String fund,
        double lastPrice,
        String lastPriceDate,
        int pledgedQuantity,
        int pnl,
        double quantity,
        String tradingsymbol,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}
