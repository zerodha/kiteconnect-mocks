// To parse this JSON data, do
//
//     final sessionLogout = sessionLogoutFromJson(jsonString);

import 'dart:convert';

SessionLogout sessionLogoutFromJson(String str) => SessionLogout.fromJson(json.decode(str));

String sessionLogoutToJson(SessionLogout data) => json.encode(data.toJson());

class SessionLogout {
    SessionLogout({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory SessionLogout.fromJson(Map<String, dynamic> json) => SessionLogout(
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
        this.sessionLogout,
    });

    SessionLogoutClass sessionLogout;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        sessionLogout: SessionLogoutClass.fromJson(json["SessionLogout"]),
    );

    Map<String, dynamic> toJson() => {
        "SessionLogout": sessionLogout.toJson(),
    };
}

class SessionLogoutClass {
    SessionLogoutClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    Properties properties;
    List<String> required;
    String title;
    String type;

    factory SessionLogoutClass.fromJson(Map<String, dynamic> json) => SessionLogoutClass(
        additionalProperties: json["additionalProperties"],
        properties: Properties.fromJson(json["properties"]),
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

class Properties {
    Properties({
        this.data,
        this.status,
    });

    Data data;
    Data status;

    factory Properties.fromJson(Map<String, dynamic> json) => Properties(
        data: Data.fromJson(json["data"]),
        status: Data.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
    };
}

class Data {
    Data({
        this.type,
    });

    String type;

    factory Data.fromJson(Map<String, dynamic> json) => Data(
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "type": type,
    };
}
