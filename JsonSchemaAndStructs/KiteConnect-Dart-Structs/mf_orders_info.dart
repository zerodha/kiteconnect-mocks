// To parse this JSON data, do
//
//     final mfOrdersInfo = mfOrdersInfoFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_orders_info.freezed.dart';
part 'mf_orders_info.g.dart';

@freezed
abstract class MfOrdersInfo with _$MfOrdersInfo {
    const factory MfOrdersInfo({
        Data data,
        String status,
    }) = _MfOrdersInfo;

    factory MfOrdersInfo.fromJson(Map<String, dynamic> json) => _$MfOrdersInfoFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        int amount,
        int averagePrice,
        dynamic exchangeOrderId,
        dynamic exchangeTimestamp,
        dynamic folio,
        String fund,
        double lastPrice,
        DateTime lastPriceDate,
        String orderId,
        DateTime orderTimestamp,
        String placedBy,
        String purchaseType,
        int quantity,
        dynamic settlementId,
        String status,
        String statusMessage,
        dynamic tag,
        String tradingsymbol,
        String transactionType,
        String variety,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}
