// To parse this JSON data, do
//
//     final postback = postbackFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'postback.freezed.dart';
part 'postback.g.dart';

@freezed
abstract class Postback with _$Postback {
    const factory Postback({
        int appId,
        int averagePrice,
        int cancelledQuantity,
        String checksum,
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
        dynamic statusMessage,
        dynamic statusMessageRaw,
        dynamic tag,
        String tradingsymbol,
        String transactionType,
        int triggerPrice,
        int unfilledQuantity,
        String userId,
        String validity,
        String variety,
    }) = _Postback;

    factory Postback.fromJson(Map<String, dynamic> json) => _$PostbackFromJson(json);
}

@freezed
abstract class Meta with _$Meta {
    const factory Meta() = _Meta;

    factory Meta.fromJson(Map<String, dynamic> json) => _$MetaFromJson(json);
}
