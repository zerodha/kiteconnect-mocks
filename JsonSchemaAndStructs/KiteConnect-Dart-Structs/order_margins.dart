// To parse this JSON data, do
//
//     final orderMargins = orderMarginsFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'order_margins.freezed.dart';
part 'order_margins.g.dart';

@freezed
abstract class OrderMargins with _$OrderMargins {
    const factory OrderMargins({
        List<Datum> data,
        String status,
    }) = _OrderMargins;

    factory OrderMargins.fromJson(Map<String, dynamic> json) => _$OrderMarginsFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        int additional,
        int bo,
        int cash,
        String exchange,
        int exposure,
        int optionPremium,
        Pnl pnl,
        int span,
        double total,
        String tradingsymbol,
        String type,
        double datumVar,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}

@freezed
abstract class Pnl with _$Pnl {
    const factory Pnl({
        int realised,
        int unrealised,
    }) = _Pnl;

    factory Pnl.fromJson(Map<String, dynamic> json) => _$PnlFromJson(json);
}
