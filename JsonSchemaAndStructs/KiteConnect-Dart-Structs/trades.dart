// To parse this JSON data, do
//
//     final trades = tradesFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'trades.freezed.dart';
part 'trades.g.dart';

@freezed
abstract class Trades with _$Trades {
    const factory Trades({
        List<Datum> data,
        String status,
    }) = _Trades;

    factory Trades.fromJson(Map<String, dynamic> json) => _$TradesFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        double averagePrice,
        String exchange,
        String exchangeOrderId,
        DateTime exchangeTimestamp,
        DateTime fillTimestamp,
        int instrumentToken,
        String orderId,
        String orderTimestamp,
        String product,
        int quantity,
        String tradeId,
        String tradingsymbol,
        String transactionType,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}
