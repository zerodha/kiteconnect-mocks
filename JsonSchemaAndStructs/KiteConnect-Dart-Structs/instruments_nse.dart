// To parse this JSON data, do
//
//     final instrumentsNse = instrumentsNseFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'instruments_nse.freezed.dart';
part 'instruments_nse.g.dart';

@freezed
abstract class InstrumentsNse with _$InstrumentsNse {
    const factory InstrumentsNse({
        Exchange exchange,
        int exchangeToken,
        String expiry,
        int instrumentToken,
        InstrumentType instrumentType,
        int lastPrice,
        int lotSize,
        String name,
        Exchange segment,
        int strike,
        double tickSize,
        String tradingsymbol,
    }) = _InstrumentsNse;

    factory InstrumentsNse.fromJson(Map<String, dynamic> json) => _$InstrumentsNseFromJson(json);
}

enum Exchange { NSE }

final exchangeValues = EnumValues({
    "NSE": Exchange.NSE
});

enum InstrumentType { EQ }

final instrumentTypeValues = EnumValues({
    "EQ": InstrumentType.EQ
});

class EnumValues<T> {
    Map<String, T> map;
    Map<T, String> reverseMap;

    EnumValues(this.map);

    Map<T, String> get reverse {
        if (reverseMap == null) {
            reverseMap = map.map((k, v) => new MapEntry(v, k));
        }
        return reverseMap;
    }
}
