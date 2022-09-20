// To parse this JSON data, do
//
//     final instrumentsAll = instrumentsAllFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'instruments_all.freezed.dart';
part 'instruments_all.g.dart';

@freezed
abstract class InstrumentsAll with _$InstrumentsAll {
    const factory InstrumentsAll({
        Exchange exchange,
        int exchangeToken,
        String expiry,
        int instrumentToken,
        InstrumentType instrumentType,
        double lastPrice,
        int lotSize,
        String name,
        Segment segment,
        int strike,
        double tickSize,
        String tradingsymbol,
    }) = _InstrumentsAll;

    factory InstrumentsAll.fromJson(Map<String, dynamic> json) => _$InstrumentsAllFromJson(json);
}

enum Exchange { NSE, NFO, BSE }

final exchangeValues = EnumValues({
    "BSE": Exchange.BSE,
    "NFO": Exchange.NFO,
    "NSE": Exchange.NSE
});

enum InstrumentType { EQ, CE, PE }

final instrumentTypeValues = EnumValues({
    "CE": InstrumentType.CE,
    "EQ": InstrumentType.EQ,
    "PE": InstrumentType.PE
});

enum Segment { NSE, NFO_OPT, BSE }

final segmentValues = EnumValues({
    "BSE": Segment.BSE,
    "NFO-OPT": Segment.NFO_OPT,
    "NSE": Segment.NSE
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
