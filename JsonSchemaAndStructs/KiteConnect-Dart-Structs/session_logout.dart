// To parse this JSON data, do
//
//     final sessionLogout = sessionLogoutFromMap(jsonString);

import 'package:freezed_annotation/freezed_annotation.dart';
import 'dart:convert';

part 'session_logout.freezed.dart';
part 'session_logout.g.dart';

@freezed
abstract class SessionLogout with _$SessionLogout {
    const factory SessionLogout({
        bool data,
        String status,
    }) = _SessionLogout;

    factory SessionLogout.fromJson(Map<String, dynamic> json) => _$SessionLogoutFromJson(json);
}
