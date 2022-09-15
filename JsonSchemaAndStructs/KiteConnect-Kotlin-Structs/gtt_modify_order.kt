// To parse the JSON, install Klaxon and do:
//
//   val gttModifyOrder = GttModifyOrder.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class GttModifyOrder (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<GttModifyOrder>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "GttModifyOrder")
    val gttModifyOrder: GttModifyOrderClass
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    @Json(name = "trigger_id")
    val triggerID: TriggerID
)

data class TriggerID (
    val type: String
)

data class GttModifyOrderClass (
    val additionalProperties: Boolean,
    val properties: GttModifyOrderProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class GttModifyOrderProperties (
    val data: DataClass,
    val status: TriggerID
)

data class DataClass (
    @Json(name = "\$ref")
    val ref: String
)
