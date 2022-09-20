// To parse this JSON data, do
//
//     final mfOrders = mfOrdersFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_orders.freezed.dart';
part 'mf_orders.g.dart';

@freezed
abstract class MfOrders with _$MfOrders {
    const factory MfOrders({
        List<Datum> data,
        String status,
    }) = _MfOrders;

    factory MfOrders.fromJson(Map<String, dynamic> json) => _$MfOrdersFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        int amount,
        int averagePrice,
        String exchangeOrderId,
        DateTime exchangeTimestamp,
        dynamic folio,
        String fund,
        double lastPrice,
        DateTime lastPriceDate,
        String orderId,
        DateTime orderTimestamp,
        String placedBy,
        String purchaseType,
        int quantity,
        String settlementId,
        String status,
        String statusMessage,
        String tag,
        String tradingsymbol,
        String transactionType,
        String variety,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}
