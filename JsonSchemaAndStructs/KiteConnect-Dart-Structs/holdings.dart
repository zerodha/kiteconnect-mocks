// To parse this JSON data, do
//
//     final holdings = holdingsFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'holdings.freezed.dart';
part 'holdings.g.dart';

@freezed
abstract class Holdings with _$Holdings {
    const factory Holdings({
        List<Datum> data,
        String status,
    }) = _Holdings;

    factory Holdings.fromJson(Map<String, dynamic> json) => _$HoldingsFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        DateTime authorisedDate,
        int authorisedQuantity,
        double averagePrice,
        double closePrice,
        int collateralQuantity,
        String collateralType,
        double dayChange,
        double dayChangePercentage,
        bool discrepancy,
        String exchange,
        int instrumentToken,
        String isin,
        double lastPrice,
        int openingQuantity,
        double pnl,
        int price,
        String product,
        int quantity,
        int realisedQuantity,
        int t1Quantity,
        String tradingsymbol,
        int usedQuantity,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}
