// To parse this JSON data, do
//
//     final gttGetOrders = gttGetOrdersFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'gtt_get_order.freezed.dart';
part 'gtt_get_order.g.dart';

@freezed
abstract class GttGetOrders with _$GttGetOrders {
    const factory GttGetOrders({
        List<Datum> data,
        String status,
    }) = _GttGetOrders;

    factory GttGetOrders.fromJson(Map<String, dynamic> json) => _$GttGetOrdersFromJson(json);
}

@freezed
abstract class Datum with _$Datum {
    const factory Datum({
        Condition condition,
        DateTime createdAt,
        DateTime expiresAt,
        int id,
        Meta meta,
        List<Order> orders,
        dynamic parentTrigger,
        String status,
        String type,
        DateTime updatedAt,
        String userId,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}

@freezed
abstract class Condition with _$Condition {
    const factory Condition({
        String exchange,
        int instrumentToken,
        double lastPrice,
        String tradingsymbol,
        List<double> triggerValues,
    }) = _Condition;

    factory Condition.fromJson(Map<String, dynamic> json) => _$ConditionFromJson(json);
}

@freezed
abstract class Meta with _$Meta {
    const factory Meta() = _Meta;

    factory Meta.fromJson(Map<String, dynamic> json) => _$MetaFromJson(json);
}

@freezed
abstract class Order with _$Order {
    const factory Order({
        String exchange,
        String orderType,
        double price,
        String product,
        int quantity,
        Result result,
        String tradingsymbol,
        String transactionType,
    }) = _Order;

    factory Order.fromJson(Map<String, dynamic> json) => _$OrderFromJson(json);
}

@freezed
abstract class Result with _$Result {
    const factory Result({
        String accountId,
        String exchange,
        String meta,
        OrderResult orderResult,
        String orderType,
        int price,
        String product,
        int quantity,
        DateTime timestamp,
        String tradingsymbol,
        String transactionType,
        double triggeredAt,
        String validity,
    }) = _Result;

    factory Result.fromJson(Map<String, dynamic> json) => _$ResultFromJson(json);
}

@freezed
abstract class OrderResult with _$OrderResult {
    const factory OrderResult({
        String orderId,
        String rejectionReason,
        String status,
    }) = _OrderResult;

    factory OrderResult.fromJson(Map<String, dynamic> json) => _$OrderResultFromJson(json);
}
