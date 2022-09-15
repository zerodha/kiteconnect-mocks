// To parse the JSON, install Klaxon and do:
//
//   val marginsEquity = MarginsEquity.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class MarginsEquity (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<MarginsEquity>(json)
    }
}

data class Definitions (
    @Json(name = "Available")
    val available: Available,

    @Json(name = "Data")
    val data: Data,

    @Json(name = "MarginsEquity")
    val marginsEquity: MarginsEquityClass
)

data class Available (
    val additionalProperties: Boolean,
    val properties: AvailableProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class AvailableProperties (
    @Json(name = "adhoc_margin")
    val adhocMargin: AdhocMargin,

    val cash: AdhocMargin,
    val collateral: AdhocMargin,

    @Json(name = "intraday_payin")
    val intradayPayin: AdhocMargin,

    @Json(name = "live_balance")
    val liveBalance: AdhocMargin,

    @Json(name = "opening_balance")
    val openingBalance: AdhocMargin
)

data class AdhocMargin (
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
    val available: AvailableClass,
    val enabled: AdhocMargin,
    val net: AdhocMargin,
    val utilised: Utilised
)

data class AvailableClass (
    @Json(name = "\$ref")
    val ref: String
)

data class Utilised (
    val additionalProperties: AdhocMargin,
    val type: String
)

data class MarginsEquityClass (
    val additionalProperties: Boolean,
    val properties: MarginsEquityProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class MarginsEquityProperties (
    val data: AvailableClass,
    val status: AdhocMargin
)
