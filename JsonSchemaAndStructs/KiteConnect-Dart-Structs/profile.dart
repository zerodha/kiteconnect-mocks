// To parse this JSON data, do
//
//     final profile = profileFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'profile.freezed.dart';
part 'profile.g.dart';

@freezed
abstract class Profile with _$Profile {
    const factory Profile({
        Data data,
        String status,
    }) = _Profile;

    factory Profile.fromJson(Map<String, dynamic> json) => _$ProfileFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        dynamic avatarUrl,
        String broker,
        String email,
        List<String> exchanges,
        Meta meta,
        List<String> orderTypes,
        List<String> products,
        String userId,
        String userName,
        String userShortname,
        String userType,
    }) = _Data;

    factory Data.fromJson(Map<String, dynamic> json) => _$DataFromJson(json);
}

@freezed
abstract class Meta with _$Meta {
    const factory Meta({
        String dematConsent,
    }) = _Meta;

    factory Meta.fromJson(Map<String, dynamic> json) => _$MetaFromJson(json);
}
