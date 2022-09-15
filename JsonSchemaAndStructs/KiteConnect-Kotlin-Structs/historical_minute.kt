// To parse the JSON, install Klaxon and do:
//
//   val historicalMinute = HistoricalMinute.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class HistoricalMinute (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<HistoricalMinute>(json)
    }
}

data class Definitions (
    @Json(name = "Candle")
    val candle: Candle,

    @Json(name = "Data")
    val data: Data,

    @Json(name = "HistoricalMinute")
    val historicalMinute: HistoricalMinuteClass
)

data class Candle (
    val anyOf: List<AnyOf>,
    val title: String
)

data class AnyOf (
    val type: String
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    val candles: Candles
)

data class Candles (
    val items: Items,
    val type: String
)

data class Items (
    val items: DataClass,
    val type: String
)

data class DataClass (
    @Json(name = "\$ref")
    val ref: String
)

data class HistoricalMinuteClass (
    val additionalProperties: Boolean,
    val properties: HistoricalMinuteProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class HistoricalMinuteProperties (
    val data: DataClass,
    val status: AnyOf
)
