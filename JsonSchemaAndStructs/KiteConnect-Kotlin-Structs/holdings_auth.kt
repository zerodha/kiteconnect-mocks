// To parse the JSON, install Klaxon and do:
//
//   val holdingsAuth = HoldingsAuth.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class HoldingsAuth (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<HoldingsAuth>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "HoldingsAuth")
    val holdingsAuth: HoldingsAuthClass
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    @Json(name = "request_id")
    val requestID: RequestID
)

data class RequestID (
    val type: String
)

data class HoldingsAuthClass (
    val additionalProperties: Boolean,
    val properties: HoldingsAuthProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class HoldingsAuthProperties (
    val data: DataClass,
    val status: RequestID
)

data class DataClass (
    @Json(name = "\$ref")
    val ref: String
)
