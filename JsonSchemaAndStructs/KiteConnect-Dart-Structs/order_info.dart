// To parse this JSON data, do
//
//     final orderInfo = orderInfoFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'order_info.freezed.dart';
part 'order_info.g.dart';

@freezed
abstract class OrderInfo with _$OrderInfo {
    const factory OrderInfo({
        List<Datum> data,
        String status,
    }) = _OrderInfo;

    factory OrderInfo.fromJson(Map<String, dynamic> json) => _$OrderInfoFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        int averagePrice,
        int cancelledQuantity,
        int disclosedQuantity,
        String exchange,
        String exchangeOrderId,
        DateTime exchangeTimestamp,
        int filledQuantity,
        int instrumentToken,
        String orderId,
        DateTime orderTimestamp,
        String orderType,
        dynamic parentOrderId,
        int pendingQuantity,
        String placedBy,
        double price,
        String product,
        int quantity,
        String status,
        dynamic statusMessage,
        dynamic tag,
        String tradingsymbol,
        String transactionType,
        int triggerPrice,
        String validity,
        String variety,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}
