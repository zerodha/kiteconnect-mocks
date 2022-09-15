// To parse this JSON data, do
//
//     final mfSipInfo = mfSipInfoFromJson(jsonString);

import 'dart:convert';

MfSipInfo mfSipInfoFromJson(String str) => MfSipInfo.fromJson(json.decode(str));

String mfSipInfoToJson(MfSipInfo data) => json.encode(data.toJson());

class MfSipInfo {
    MfSipInfo({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory MfSipInfo.fromJson(Map<String, dynamic> json) => MfSipInfo(
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
        this.data,
        this.mfsipInfo,
        this.stepUp,
    });

    Data data;
    MfsipInfoClass mfsipInfo;
    StepUpClass stepUp;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        mfsipInfo: MfsipInfoClass.fromJson(json["MFSIPInfo"]),
        stepUp: StepUpClass.fromJson(json["StepUp"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "MFSIPInfo": mfsipInfo.toJson(),
        "StepUp": stepUp.toJson(),
    };
}

class Data {
    Data({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    DataProperties properties;
    List<String> required;
    String title;
    String type;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
        additionalProperties: json["additionalProperties"],
        properties: DataProperties.fromJson(json["properties"]),
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

class DataProperties {
    DataProperties({
        this.completedInstalments,
        this.created,
        this.dividendType,
        this.frequency,
        this.fund,
        this.fundSource,
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
    CompletedInstalments fundSource;
    CompletedInstalments instalmentAmount;
    CompletedInstalments instalmentDay;
    CompletedInstalments instalments;
    Created lastInstalment;
    Created nextInstalment;
    CompletedInstalments pendingInstalments;
    CompletedInstalments sipId;
    CompletedInstalments sipRegNum;
    CompletedInstalments sipType;
    CompletedInstalments status;
    StepUp stepUp;
    CompletedInstalments tag;
    CompletedInstalments tradingsymbol;
    CompletedInstalments transactionType;
    CompletedInstalments triggerPrice;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        completedInstalments: CompletedInstalments.fromJson(json["completed_instalments"]),
        created: Created.fromJson(json["created"]),
        dividendType: CompletedInstalments.fromJson(json["dividend_type"]),
        frequency: CompletedInstalments.fromJson(json["frequency"]),
        fund: CompletedInstalments.fromJson(json["fund"]),
        fundSource: CompletedInstalments.fromJson(json["fund_source"]),
        instalmentAmount: CompletedInstalments.fromJson(json["instalment_amount"]),
        instalmentDay: CompletedInstalments.fromJson(json["instalment_day"]),
        instalments: CompletedInstalments.fromJson(json["instalments"]),
        lastInstalment: Created.fromJson(json["last_instalment"]),
        nextInstalment: Created.fromJson(json["next_instalment"]),
        pendingInstalments: CompletedInstalments.fromJson(json["pending_instalments"]),
        sipId: CompletedInstalments.fromJson(json["sip_id"]),
        sipRegNum: CompletedInstalments.fromJson(json["sip_reg_num"]),
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
        "fund_source": fundSource.toJson(),
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

enum Type { INTEGER, STRING, NUMBER, NULL }

final typeValues = EnumValues({
    "integer": Type.INTEGER,
    "null": Type.NULL,
    "number": Type.NUMBER,
    "string": Type.STRING
});

class Created {
    Created({
        this.format,
        this.type,
    });

    String format;
    Type type;

    factory Created.fromJson(Map<String, dynamic> json) => Created(
        format: json["format"],
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "format": format,
        "type": typeValues.reverse[type],
    };
}

class StepUp {
    StepUp({
        this.ref,
    });

    String ref;

    factory StepUp.fromJson(Map<String, dynamic> json) => StepUp(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class MfsipInfoClass {
    MfsipInfoClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MfsipInfoProperties properties;
    List<String> required;
    String title;
    String type;

    factory MfsipInfoClass.fromJson(Map<String, dynamic> json) => MfsipInfoClass(
        additionalProperties: json["additionalProperties"],
        properties: MfsipInfoProperties.fromJson(json["properties"]),
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

class MfsipInfoProperties {
    MfsipInfoProperties({
        this.data,
        this.status,
    });

    StepUp data;
    CompletedInstalments status;

    factory MfsipInfoProperties.fromJson(Map<String, dynamic> json) => MfsipInfoProperties(
        data: StepUp.fromJson(json["data"]),
        status: CompletedInstalments.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class StepUpClass {
    StepUpClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    StepUpProperties properties;
    List<String> required;
    String title;
    String type;

    factory StepUpClass.fromJson(Map<String, dynamic> json) => StepUpClass(
        additionalProperties: json["additionalProperties"],
        properties: StepUpProperties.fromJson(json["properties"]),
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

class StepUpProperties {
    StepUpProperties({
        this.the1502,
    });

    CompletedInstalments the1502;

    factory StepUpProperties.fromJson(Map<String, dynamic> json) => StepUpProperties(
        the1502: CompletedInstalments.fromJson(json["15-02"]),
    );

    Map<String, dynamic> toJson() => {
        "15-02": the1502.toJson(),
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
