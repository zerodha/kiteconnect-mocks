// To parse the JSON, install Klaxon and do:
//
//   val triggerRange = TriggerRange.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class TriggerRange (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<TriggerRange>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "Nse")
    val nse: Nse,

    @Json(name = "TriggerRange")
    val triggerRange: TriggerRangeClass
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
    val nseInfy: NseInfy,

    @Json(name = "NSE:RELIANCE")
    val nseReliance: NseInfy
)

data class NseInfy (
    @Json(name = "\$ref")
    val ref: String
)

data class Nse (
    val additionalProperties: Boolean,
    val properties: NseProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class NseProperties (
    @Json(name = "instrument_token")
    val instrumentToken: InstrumentToken,

    val lower: InstrumentToken,
    val upper: InstrumentToken
)

data class InstrumentToken (
    val type: String
)

data class TriggerRangeClass (
    val additionalProperties: Boolean,
    val properties: TriggerRangeProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class TriggerRangeProperties (
    val data: NseInfy,
    val status: InstrumentToken
)
