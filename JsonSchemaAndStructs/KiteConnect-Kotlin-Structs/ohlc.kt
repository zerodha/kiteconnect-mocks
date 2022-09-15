// To parse the JSON, install Klaxon and do:
//
//   val ohlc = Ohlc.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class Ohlc (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<Ohlc>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "NseInfy")
    val nseInfy: NseInfyClass,

    @Json(name = "Ohlc")
    val ohlc: OhlcClass,

    @Json(name = "OhlcClass")
    val ohlcClass: OhlcClassClass
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

data class NseInfyClass (
    val additionalProperties: Boolean,
    val properties: NseInfyProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class NseInfyProperties (
    @Json(name = "instrument_token")
    val instrumentToken: InstrumentToken,

    @Json(name = "last_price")
    val lastPrice: InstrumentToken,

    val ohlc: NseInfy
)

data class InstrumentToken (
    val type: String
)

data class OhlcClass (
    val additionalProperties: Boolean,
    val properties: OhlcProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OhlcProperties (
    val data: NseInfy,
    val status: InstrumentToken
)

data class OhlcClassClass (
    val additionalProperties: Boolean,
    val properties: OhlcClassProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OhlcClassProperties (
    val close: InstrumentToken,
    val high: InstrumentToken,
    val low: InstrumentToken,
    val open: InstrumentToken
)
