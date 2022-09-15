// To parse the JSON, install Klaxon and do:
//
//   val orderInfo = OrderInfo.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private fun <T> Klaxon.convert(k: kotlin.reflect.KClass<*>, fromJson: (JsonValue) -> T, toJson: (T) -> String, isUnion: Boolean = false) =
    this.converter(object: Converter {
        @Suppress("UNCHECKED_CAST")
        override fun toJson(value: Any)        = toJson(value as T)
        override fun fromJson(jv: JsonValue)   = fromJson(jv) as Any
        override fun canConvert(cls: Class<*>) = cls == k.java || (isUnion && cls.superclass == k.java)
    })

private val klaxon = Klaxon()
    .convert(Type::class, { Type.fromValue(it.string!!) }, { "\"${it.value}\"" })

data class OrderInfo (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<OrderInfo>(json)
    }
}

data class Definitions (
    @Json(name = "Datum")
    val datum: Datum,

    @Json(name = "OrderInfo")
    val orderInfo: OrderInfoClass
)

data class Datum (
    val additionalProperties: Boolean,
    val properties: DatumProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DatumProperties (
    @Json(name = "average_price")
    val averagePrice: AveragePrice,

    @Json(name = "cancelled_quantity")
    val cancelledQuantity: AveragePrice,

    @Json(name = "disclosed_quantity")
    val disclosedQuantity: AveragePrice,

    val exchange: AveragePrice,

    @Json(name = "exchange_order_id")
    val exchangeOrderID: ExchangeOrderID,

    @Json(name = "exchange_timestamp")
    val exchangeTimestamp: ExchangeTimestamp,

    @Json(name = "filled_quantity")
    val filledQuantity: AveragePrice,

    @Json(name = "instrument_token")
    val instrumentToken: AveragePrice,

    @Json(name = "order_id")
    val orderID: AveragePrice,

    @Json(name = "order_timestamp")
    val orderTimestamp: OrderTimestamp,

    @Json(name = "order_type")
    val orderType: AveragePrice,

    @Json(name = "parent_order_id")
    val parentOrderID: AveragePrice,

    @Json(name = "pending_quantity")
    val pendingQuantity: AveragePrice,

    @Json(name = "placed_by")
    val placedBy: AveragePrice,

    val price: AveragePrice,
    val product: AveragePrice,
    val quantity: AveragePrice,
    val status: AveragePrice,

    @Json(name = "status_message")
    val statusMessage: AveragePrice,

    val tag: AveragePrice,
    val tradingsymbol: AveragePrice,

    @Json(name = "transaction_type")
    val transactionType: AveragePrice,

    @Json(name = "trigger_price")
    val triggerPrice: AveragePrice,

    val validity: AveragePrice,
    val variety: AveragePrice
)

data class AveragePrice (
    val type: Type
)

enum class Type(val value: String) {
    Integer("integer"),
    Null("null"),
    Number("number"),
    TypeString("string");

    companion object {
        public fun fromValue(value: String): Type = when (value) {
            "integer" -> Integer
            "null"    -> Null
            "number"  -> Number
            "string"  -> TypeString
            else      -> throw IllegalArgumentException()
        }
    }
}

data class ExchangeOrderID (
    val anyOf: List<AveragePrice>
)

data class ExchangeTimestamp (
    val anyOf: List<OrderTimestamp>
)

data class OrderTimestamp (
    val format: String? = null,
    val type: Type
)

data class OrderInfoClass (
    val additionalProperties: Boolean,
    val properties: OrderInfoProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OrderInfoProperties (
    val data: Data,
    val status: AveragePrice
)

data class Data (
    val items: Items,
    val type: String
)

data class Items (
    @Json(name = "\$ref")
    val ref: String
)
