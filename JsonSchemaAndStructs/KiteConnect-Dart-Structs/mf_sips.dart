// To parse this JSON data, do
//
//     final mfSips = mfSipsFromJson(jsonString);

import 'dart:convert';

MfSips mfSipsFromJson(String str) => MfSips.fromJson(json.decode(str));

String mfSipsToJson(MfSips data) => json.encode(data.toJson());

class MfSips {
    MfSips({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfSips.fromJson(Map<String, dynamic> json) => MfSips(
        ref: json["\u0024ref"],
        schema: json["\u0024schema"],
        definitions: Definitions.fromJson(json["definitions"]),
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
        "\u0024schema": schema,
        "definitions": definitions.toJson(),
    };
}

class Definitions {
    Definitions({
        this.datum,
        this.mfSips,
    });

    Datum datum;
    MfSipsClass mfSips;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        datum: Datum.fromJson(json["Datum"]),
        mfSips: MfSipsClass.fromJson(json["MFSips"]),
    );

    Map<String, dynamic> toJson() => {
        "Datum": datum.toJson(),
        "MFSips": mfSips.toJson(),
    };
}

class Datum {
    Datum({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    DatumProperties properties;
    List<String> required;
    String title;
    String type;

    factory Datum.fromJson(Map<String, dynamic> json) => Datum(
        additionalProperties: json["additionalProperties"],
        properties: DatumProperties.fromJson(json["properties"]),
        required: List<String>.from(json["required"].map((x) => x)),
        title: json["title"],
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "additionalProperties": additionalProperties,
        "properties": properties.toJson(),
        "required": List<dynamic>.from(required.map((x) => x)),
        "title": title,
        "type": type,
    };
}

class DatumProperties {
    DatumProperties({
        this.completedInstalments,
        this.created,
        this.dividendType,
        this.frequency,
        this.fund,
        this.instalmentAmount,
        this.instalmentDay,
        this.instalments,
        this.lastInstalment,
        this.nextInstalment,
        this.pendingInstalments,
        this.sipId,
        this.sipRegNum,
        this.sipType,
        this.status,
        this.stepUp,
        this.tag,
        this.tradingsymbol,
        this.transactionType,
        this.triggerPrice,
    });

    CompletedInstalments completedInstalments;
    Created created;
    CompletedInstalments dividendType;
    CompletedInstalments frequency;
    CompletedInstalments fund;
    CompletedInstalments instalmentAmount;
    CompletedInstalments instalmentDay;
    CompletedInstalments instalments;
    Created lastInstalment;
    Created nextInstalment;
    CompletedInstalments pendingInstalments;
    CompletedInstalments sipId;
    SipRegNum sipRegNum;
    CompletedInstalments sipType;
    CompletedInstalments status;
    StepUp stepUp;
    CompletedInstalments tag;
    CompletedInstalments tradingsymbol;
    CompletedInstalments transactionType;
    CompletedInstalments triggerPrice;

    factory DatumProperties.fromJson(Map<String, dynamic> json) => DatumProperties(
        completedInstalments: CompletedInstalments.fromJson(json["completed_instalments"]),
        created: Created.fromJson(json["created"]),
        dividendType: CompletedInstalments.fromJson(json["dividend_type"]),
        frequency: CompletedInstalments.fromJson(json["frequency"]),
        fund: CompletedInstalments.fromJson(json["fund"]),
        instalmentAmount: CompletedInstalments.fromJson(json["instalment_amount"]),
        instalmentDay: CompletedInstalments.fromJson(json["instalment_day"]),
        instalments: CompletedInstalments.fromJson(json["instalments"]),
        lastInstalment: Created.fromJson(json["last_instalment"]),
        nextInstalment: Created.fromJson(json["next_instalment"]),
        pendingInstalments: CompletedInstalments.fromJson(json["pending_instalments"]),
        sipId: CompletedInstalments.fromJson(json["sip_id"]),
        sipRegNum: SipRegNum.fromJson(json["sip_reg_num"]),
        sipType: CompletedInstalments.fromJson(json["sip_type"]),
        status: CompletedInstalments.fromJson(json["status"]),
        stepUp: StepUp.fromJson(json["step_up"]),
        tag: CompletedInstalments.fromJson(json["tag"]),
        tradingsymbol: CompletedInstalments.fromJson(json["tradingsymbol"]),
        transactionType: CompletedInstalments.fromJson(json["transaction_type"]),
        triggerPrice: CompletedInstalments.fromJson(json["trigger_price"]),
    );

    Map<String, dynamic> toJson() => {
        "completed_instalments": completedInstalments.toJson(),
        "created": created.toJson(),
        "dividend_type": dividendType.toJson(),
        "frequency": frequency.toJson(),
        "fund": fund.toJson(),
        "instalment_amount": instalmentAmount.toJson(),
        "instalment_day": instalmentDay.toJson(),
        "instalments": instalments.toJson(),
        "last_instalment": lastInstalment.toJson(),
        "next_instalment": nextInstalment.toJson(),
        "pending_instalments": pendingInstalments.toJson(),
        "sip_id": sipId.toJson(),
        "sip_reg_num": sipRegNum.toJson(),
        "sip_type": sipType.toJson(),
        "status": status.toJson(),
        "step_up": stepUp.toJson(),
        "tag": tag.toJson(),
        "tradingsymbol": tradingsymbol.toJson(),
        "transaction_type": transactionType.toJson(),
        "trigger_price": triggerPrice.toJson(),
    };
}

class CompletedInstalments {
    CompletedInstalments({
        this.type,
    });

    Type type;

    factory CompletedInstalments.fromJson(Map<String, dynamic> json) => CompletedInstalments(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { INTEGER, STRING, NUMBER }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class Created {
    Created({
        this.format,
        this.type,
    });

    String format;
    String type;

    factory Created.fromJson(Map<String, dynamic> json) => Created(
        format: json["format"] == null ? null : json["format"],
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "format": format == null ? null : format,
        "type": type,
    };
}

class SipRegNum {
    SipRegNum({
        this.anyOf,
    });

    List<Created> anyOf;

    factory SipRegNum.fromJson(Map<String, dynamic> json) => SipRegNum(
        anyOf: List<Created>.from(json["anyOf"].map((x) => Created.fromJson(x))),
    );

    Map<String, dynamic> toJson() => {
        "anyOf": List<dynamic>.from(anyOf.map((x) => x.toJson())),
    };
}

class StepUp {
    StepUp({
        this.additionalProperties,
        this.type,
    });

    CompletedInstalments additionalProperties;
    String type;

    factory StepUp.fromJson(Map<String, dynamic> json) => StepUp(
        additionalProperties: CompletedInstalments.fromJson(json["additionalProperties"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "additionalProperties": additionalProperties.toJson(),
        "type": type,
    };
}

class MfSipsClass {
    MfSipsClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfSipsProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfSipsClass.fromJson(Map<String, dynamic> json) => MfSipsClass(
        additionalProperties: json["additionalProperties"],
        properties: MfSipsProperties.fromJson(json["properties"]),
        required: List<String>.from(json["required"].map((x) => x)),
        title: json["title"],
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "additionalProperties": additionalProperties,
        "properties": properties.toJson(),
        "required": List<dynamic>.from(required.map((x) => x)),
        "title": title,
        "type": type,
    };
}

class MfSipsProperties {
    MfSipsProperties({
        this.data,
    });

    Data data;

    factory MfSipsProperties.fromJson(Map<String, dynamic> json) => MfSipsProperties(
        data: Data.fromJson(json["data"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
    };
}

class Data {
    Data({
        this.items,
        this.type,
    });

    Items items;
    String type;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
        items: Items.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
    };
}

class Items {
    Items({
        this.ref,
    });

    String ref;

    factory Items.fromJson(Map<String, dynamic> json) => Items(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

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
