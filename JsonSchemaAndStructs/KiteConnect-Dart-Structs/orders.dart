// To parse this JSON data, do
//
//     final orders = ordersFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'orders.freezed.dart';
part 'orders.g.dart';

@freezed
abstract class Orders with _$Orders {
    const factory Orders({
        List<Datum> data,
        String status,
    }) = _Orders;

    factory Orders.fromJson(Map<String, dynamic> json) => _$OrdersFromJson(json);
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
        DateTime exchangeUpdateTimestamp,
        int filledQuantity,
        String guid,
        int instrumentToken,
        int marketProtection,
        Meta meta,
        bool modified,
        String orderId,
        DateTime orderTimestamp,
        String orderType,
        dynamic parentOrderId,
        int pendingQuantity,
        String placedBy,
        int price,
        String product,
        int quantity,
        String status,
        String statusMessage,
        String statusMessageRaw,
        String tag,
        List<String> tags,
        String tradingsymbol,
        String transactionType,
        int triggerPrice,
        String validity,
        int validityTtl,
        String variety,
    }) = _Datum;

    factory Datum.fromJson(Map<String, dynamic> json) => _$DatumFromJson(json);
}

@freezed
abstract class Meta with _$Meta {
    const factory Meta({
        Iceberg iceberg,
    }) = _Meta;

    factory Meta.fromJson(Map<String, dynamic> json) => _$MetaFromJson(json);
}

@freezed
abstract class Iceberg with _$Iceberg {
    const factory Iceberg({
        int leg,
        int legQuantity,
        int legs,
        int remainingQuantity,
        int totalQuantity,
    }) = _Iceberg;

    factory Iceberg.fromJson(Map<String, dynamic> json) => _$IcebergFromJson(json);
}
