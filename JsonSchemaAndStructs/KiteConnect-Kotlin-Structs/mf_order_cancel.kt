// To parse the JSON, install Klaxon and do:
//
//   val mFOrderCancel = MFOrderCancel.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class MFOrderCancel (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<MFOrderCancel>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "MFOrderCancel")
    val mfOrderCancel: MFOrderCancelClass
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    @Json(name = "order_id")
    val orderID: OrderID
)

data class OrderID (
    val format: String,
    val type: String
)

data class MFOrderCancelClass (
    val additionalProperties: Boolean,
    val properties: MFOrderCancelProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class MFOrderCancelProperties (
    val data: DataClass,
    val status: Status
)

data class DataClass (
    @Json(name = "\$ref")
    val ref: String
)

data class Status (
    val type: String
)
