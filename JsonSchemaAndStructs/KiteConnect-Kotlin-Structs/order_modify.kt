// To parse the JSON, install Klaxon and do:
//
//   val orderModify = OrderModify.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class OrderModify (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<OrderModify>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "OrderModify")
    val orderModify: OrderModifyClass
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

data class OrderModifyClass (
    val additionalProperties: Boolean,
    val properties: OrderModifyProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OrderModifyProperties (
    val data: DataClass,
    val status: OrderID
)

data class DataClass (
    @Json(name = "\$ref")
    val ref: String
)
