// To parse the JSON, install Klaxon and do:
//
//   val sessionLogout = SessionLogout.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class SessionLogout (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<SessionLogout>(json)
    }
}

data class Definitions (
    @Json(name = "SessionLogout")
    val sessionLogout: SessionLogoutClass
)

data class SessionLogoutClass (
    val additionalProperties: Boolean,
    val properties: Properties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class Properties (
    val data: Data,
    val status: Data
)

data class Data (
    val type: String
)
