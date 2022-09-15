// To parse the JSON, install Klaxon and do:
//
//   val orderCancel = OrderCancel.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class OrderCancel (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<OrderCancel>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "OrderCancel")
    val orderCancel: OrderCancelClass
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
    val type: String
)

data class OrderCancelClass (
    val additionalProperties: Boolean,
    val properties: OrderCancelProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OrderCancelProperties (
    val data: DataClass,
    val status: OrderID
)

data class DataClass (
    @Json(name = "\$ref")
    val ref: String
)
