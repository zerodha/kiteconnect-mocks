// To parse this JSON data, do
//
//     final gttGetOrder = gttGetOrderFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'gtt_get_order.freezed.dart';
part 'gtt_get_order.g.dart';

@freezed
abstract class GttGetOrder with _$GttGetOrder {
    const factory GttGetOrder({
        Data data,
        String status,
    }) = _GttGetOrder;

    factory GttGetOrder.fromJson(Map<String, dynamic> json) => _$GttGetOrderFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        Condition condition,
        DateTime createdAt,
        DateTime expiresAt,
        int id,
        dynamic meta,
        List<Order> orders,
        dynamic parentTrigger,
        String status,
        String type,
        DateTime updatedAt,
        String userId,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
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
abstract class Order with _$Order {
    const factory Order({
        String exchange,
        String orderType,
        int price,
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
