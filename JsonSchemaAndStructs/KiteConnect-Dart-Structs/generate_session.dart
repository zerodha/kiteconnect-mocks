// To parse this JSON data, do
//
//     final generateSession = generateSessionFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'generate_session.freezed.dart';
part 'generate_session.g.dart';

@freezed
abstract class GenerateSession with _$GenerateSession {
    const factory GenerateSession({
        Data data,
        String status,
    }) = _GenerateSession;

    factory GenerateSession.fromJson(Map<String, dynamic> json) => _$GenerateSessionFromJson(json);
}

@freezed
abstract class Data with _$Data {
    const factory Data({
        String accessToken,
        String apiKey,
        String avatarUrl,
        String broker,
        String email,
        String enctoken,
        List<String> exchanges,
        DateTime loginTime,
        Meta meta,
        List<String> orderTypes,
        List<String> products,
        String publicToken,
        String refreshToken,
        String silo,
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
