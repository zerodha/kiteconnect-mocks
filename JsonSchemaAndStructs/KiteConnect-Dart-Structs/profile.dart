// To parse this JSON data, do
//
//     final profile = profileFromJson(jsonString);

import 'dart:convert';

Profile profileFromJson(String str) => Profile.fromJson(json.decode(str));

String profileToJson(Profile data) => json.encode(data.toJson());

class Profile {
    Profile({
        this.ref,
        this.schema,
        this.definitions,
    });

    String ref;
    String schema;
    Definitions definitions;

    factory Profile.fromJson(Map<String, dynamic> json) => Profile(
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
        this.meta,
        this.profile,
    });

    Data data;
    MetaClass meta;
    ProfileClass profile;

    factory Definitions.fromJson(Map<String, dynamic> json) => Definitions(
        data: Data.fromJson(json["Data"]),
        meta: MetaClass.fromJson(json["Meta"]),
        profile: ProfileClass.fromJson(json["Profile"]),
    );

    Map<String, dynamic> toJson() => {
        "Data": data.toJson(),
        "Meta": meta.toJson(),
        "Profile": profile.toJson(),
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
        this.avatarUrl,
        this.broker,
        this.email,
        this.exchanges,
        this.meta,
        this.orderTypes,
        this.products,
        this.userId,
        this.userName,
        this.userShortname,
        this.userType,
    });

    AvatarUrl avatarUrl;
    AvatarUrl broker;
    AvatarUrl email;
    Exchanges exchanges;
    Meta meta;
    Exchanges orderTypes;
    Exchanges products;
    AvatarUrl userId;
    AvatarUrl userName;
    AvatarUrl userShortname;
    AvatarUrl userType;

    factory DataProperties.fromJson(Map<String, dynamic> json) => DataProperties(
        avatarUrl: AvatarUrl.fromJson(json["avatar_url"]),
        broker: AvatarUrl.fromJson(json["broker"]),
        email: AvatarUrl.fromJson(json["email"]),
        exchanges: Exchanges.fromJson(json["exchanges"]),
        meta: Meta.fromJson(json["meta"]),
        orderTypes: Exchanges.fromJson(json["order_types"]),
        products: Exchanges.fromJson(json["products"]),
        userId: AvatarUrl.fromJson(json["user_id"]),
        userName: AvatarUrl.fromJson(json["user_name"]),
        userShortname: AvatarUrl.fromJson(json["user_shortname"]),
        userType: AvatarUrl.fromJson(json["user_type"]),
    );

    Map<String, dynamic> toJson() => {
        "avatar_url": avatarUrl.toJson(),
        "broker": broker.toJson(),
        "email": email.toJson(),
        "exchanges": exchanges.toJson(),
        "meta": meta.toJson(),
        "order_types": orderTypes.toJson(),
        "products": products.toJson(),
        "user_id": userId.toJson(),
        "user_name": userName.toJson(),
        "user_shortname": userShortname.toJson(),
        "user_type": userType.toJson(),
    };
}

class AvatarUrl {
    AvatarUrl({
        this.type,
    });

    Type type;

    factory AvatarUrl.fromJson(Map<String, dynamic> json) => AvatarUrl(
        type: typeValues.map[json["type"]],
    );

    Map<String, dynamic> toJson() => {
        "type": typeValues.reverse[type],
    };
}

enum Type { NULL, STRING }

final typeValues = EnumValues({
    "null": Type.NULL,
    "string": Type.STRING
});

class Exchanges {
    Exchanges({
        this.items,
        this.type,
    });

    AvatarUrl items;
    String type;

    factory Exchanges.fromJson(Map<String, dynamic> json) => Exchanges(
        items: AvatarUrl.fromJson(json["items"]),
        type: json["type"],
    );

    Map<String, dynamic> toJson() => {
        "items": items.toJson(),
        "type": type,
    };
}

class Meta {
    Meta({
        this.ref,
    });

    String ref;

    factory Meta.fromJson(Map<String, dynamic> json) => Meta(
        ref: json["\u0024ref"],
    );

    Map<String, dynamic> toJson() => {
        "\u0024ref": ref,
    };
}

class MetaClass {
    MetaClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    MetaProperties properties;
    List<String> required;
    String title;
    String type;

    factory MetaClass.fromJson(Map<String, dynamic> json) => MetaClass(
        additionalProperties: json["additionalProperties"],
        properties: MetaProperties.fromJson(json["properties"]),
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

class MetaProperties {
    MetaProperties({
        this.dematConsent,
    });

    AvatarUrl dematConsent;

    factory MetaProperties.fromJson(Map<String, dynamic> json) => MetaProperties(
        dematConsent: AvatarUrl.fromJson(json["demat_consent"]),
    );

    Map<String, dynamic> toJson() => {
        "demat_consent": dematConsent.toJson(),
    };
}

class ProfileClass {
    ProfileClass({
        this.additionalProperties,
        this.properties,
        this.required,
        this.title,
        this.type,
    });

    bool additionalProperties;
    ProfileProperties properties;
    List<String> required;
    String title;
    String type;

    factory ProfileClass.fromJson(Map<String, dynamic> json) => ProfileClass(
        additionalProperties: json["additionalProperties"],
        properties: ProfileProperties.fromJson(json["properties"]),
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

class ProfileProperties {
    ProfileProperties({
        this.data,
        this.status,
    });

    Meta data;
    AvatarUrl status;

    factory ProfileProperties.fromJson(Map<String, dynamic> json) => ProfileProperties(
        data: Meta.fromJson(json["data"]),
        status: AvatarUrl.fromJson(json["status"]),
    );

    Map<String, dynamic> toJson() => {
        "data": data.toJson(),
        "status": status.toJson(),
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
