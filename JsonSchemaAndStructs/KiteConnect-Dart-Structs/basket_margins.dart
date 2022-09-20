// To parse this JSON data, do
//
//     final basketMargins = basketMarginsFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'basket_margins.freezed.dart';
part 'basket_margins.g.dart';

@freezed
abstract class BasketMargins with _$BasketMargins {
    const factory BasketMargins({
        Data data,
        String status,
    }) = _BasketMargins;

    factory BasketMargins.fromJson(Map<String, dynamic> json) => _$BasketMarginsFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        Final dataFinal,
        Final initial,
        List<Final> orders,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}

@freezed
abstract class Final with _$Final {
    const factory Final({
        int additional,
        int bo,
        int cash,
        String exchange,
        double exposure,
        double optionPremium,
        Pnl pnl,
        double span,
        double total,
        String tradingsymbol,
        String type,
        int finalVar,
    }) = _Final;

    factory Final.fromJson(Map<String, dynamic> json) => _$FinalFromJson(json);
}

@freezed
abstract class Pnl with _$Pnl {
    const factory Pnl({
        int realised,
        int unrealised,
    }) = _Pnl;

    factory Pnl.fromJson(Map<String, dynamic> json) => _$PnlFromJson(json);
}
