// To parse this JSON data, do
//
//     final mfInstruments = mfInstrumentsFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'mf_instruments.freezed.dart';
part 'mf_instruments.g.dart';

@freezed
abstract class MfInstruments with _$MfInstruments {
    const factory MfInstruments({
        Amc amc,
        DividendType dividendType,
        double lastPrice,
        DateTime lastPriceDate,
        int minimumAdditionalPurchaseAmount,
        int minimumPurchaseAmount,
        double minimumRedemptionQuantity,
        String name,
        Plan plan,
        int purchaseAllowed,
        int purchaseAmountMultiplier,
        int redemptionAllowed,
        double redemptionQuantityMultiplier,
        SchemeType schemeType,
        SettlementType settlementType,
        String tradingsymbol,
    }) = _MfInstruments;

    factory MfInstruments.fromJson(Map<String, dynamic> json) => _$MfInstrumentsFromJson(json);
}

enum Amc { BIRLA_SUN_LIFE_MUTUAL_FUND_MF }

final amcValues = EnumValues({
    "BirlaSunLifeMutualFund_MF": Amc.BIRLA_SUN_LIFE_MUTUAL_FUND_MF
});

enum DividendType { PAYOUT, GROWTH }

final dividendTypeValues = EnumValues({
    "growth": DividendType.GROWTH,
    "payout": DividendType.PAYOUT
});

enum Plan { REGULAR, DIRECT }

final planValues = EnumValues({
    "direct": Plan.DIRECT,
    "regular": Plan.REGULAR
});

enum SchemeType { EQUITY, BALANCED, DEBT, LIQUID, FOF }

final schemeTypeValues = EnumValues({
    "balanced": SchemeType.BALANCED,
    "debt": SchemeType.DEBT,
    "equity": SchemeType.EQUITY,
    "fof": SchemeType.FOF,
    "liquid": SchemeType.LIQUID
});

enum SettlementType { T3, T1, T6, T4 }

final settlementTypeValues = EnumValues({
    "T1": SettlementType.T1,
    "T3": SettlementType.T3,
    "T4": SettlementType.T4,
    "T6": SettlementType.T6
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
