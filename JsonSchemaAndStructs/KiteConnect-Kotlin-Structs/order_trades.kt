// To parse the JSON, install Klaxon and do:
//
//   val orderTrades = OrderTrades.fromJson(jsonString)

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

data class OrderTrades (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<OrderTrades>(json)
    }
}

data class Definitions (
    @Json(name = "Datum")
    val datum: Datum,

    @Json(name = "OrderTrades")
    val orderTrades: OrderTradesClass
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

    val exchange: AveragePrice,

    @Json(name = "exchange_order_id")
    val exchangeOrderID: AveragePrice,

    @Json(name = "exchange_timestamp")
    val exchangeTimestamp: ExchangeTimestamp,

    @Json(name = "fill_timestamp")
    val fillTimestamp: ExchangeTimestamp,

    @Json(name = "instrument_token")
    val instrumentToken: AveragePrice,

    @Json(name = "order_id")
    val orderID: AveragePrice,

    @Json(name = "order_timestamp")
    val orderTimestamp: ExchangeTimestamp,

    val product: AveragePrice,
    val quantity: AveragePrice,

    @Json(name = "trade_id")
    val tradeID: ExchangeTimestamp,

    val tradingsymbol: AveragePrice,

    @Json(name = "transaction_type")
    val transactionType: AveragePrice
)

data class AveragePrice (
    val type: Type
)

enum class Type(val value: String) {
    Integer("integer"),
    TypeString("string");

    companion object {
        public fun fromValue(value: String): Type = when (value) {
            "integer" -> Integer
            "string"  -> TypeString
            else      -> throw IllegalArgumentException()
        }
    }
}

data class ExchangeTimestamp (
    val format: String,
    val type: Type
)

data class OrderTradesClass (
    val additionalProperties: Boolean,
    val properties: OrderTradesProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OrderTradesProperties (
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
