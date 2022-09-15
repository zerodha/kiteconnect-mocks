// To parse the JSON, install Klaxon and do:
//
//   val ltp = Ltp.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class Ltp (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<Ltp>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "Ltp")
    val ltp: LtpClass,

    @Json(name = "NseInfy")
    val nseInfy: NseInfyClass
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    @Json(name = "NSE:INFY")
    val nseInfy: NseInfy
)

data class NseInfy (
    @Json(name = "\$ref")
    val ref: String
)

data class LtpClass (
    val additionalProperties: Boolean,
    val properties: LtpProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class LtpProperties (
    val data: NseInfy,
    val status: Status
)

data class Status (
    val type: String
)

data class NseInfyClass (
    val additionalProperties: Boolean,
    val properties: NseInfyProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class NseInfyProperties (
    @Json(name = "instrument_token")
    val instrumentToken: Status,

    @Json(name = "last_price")
    val lastPrice: Status
)
