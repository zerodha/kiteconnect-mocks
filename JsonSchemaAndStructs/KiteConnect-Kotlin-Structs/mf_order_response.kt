// To parse the JSON, install Klaxon and do:
//
//   val mFOrderResponse = MFOrderResponse.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class MFOrderResponse (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<MFOrderResponse>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "MFOrderResponse")
    val mfOrderResponse: MFOrderResponseClass
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

data class MFOrderResponseClass (
    val additionalProperties: Boolean,
    val properties: MFOrderResponseProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class MFOrderResponseProperties (
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
