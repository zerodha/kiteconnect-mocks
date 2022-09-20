// To parse this JSON data, do
//
//     final orderTrades = orderTradesFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'order_trades.freezed.dart';
part 'order_trades.g.dart';

@freezed
abstract class OrderTrades with _$OrderTrades {
    const factory OrderTrades({
        List<Datum> data,
        String status,
    }) = _OrderTrades;

    factory OrderTrades.fromJson(Map<String, dynamic> json) => _$OrderTradesFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        int averagePrice,
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
