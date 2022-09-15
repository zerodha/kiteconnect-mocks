// To parse the JSON, install Klaxon and do:
//
//   val quote = Quote.fromJson(jsonString)

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

data class Quote (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<Quote>(json)
    }
}

data class Definitions (
    @Json(name = "Buy")
    val buy: Buy,

    @Json(name = "Data")
    val data: Data,

    @Json(name = "Depth")
    val depth: Depth,

    @Json(name = "NseInfy")
    val nseInfy: NseInfyClass,

    @Json(name = "Ohlc")
    val ohlc: Ohlc,

    @Json(name = "Quote")
    val quote: QuoteClass
)

data class Buy (
    val additionalProperties: Boolean,
    val properties: BuyProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class BuyProperties (
    val orders: Orders,
    val price: Orders,
    val quantity: Orders
)

data class Orders (
    val type: Type
)

enum class Type(val value: String) {
    Integer("integer"),
    Number("number"),
    TypeString("string");

    companion object {
        public fun fromValue(value: String): Type = when (value) {
            "integer" -> Integer
            "number"  -> Number
            "string"  -> TypeString
            else      -> throw IllegalArgumentException()
        }
    }
}

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    @Json(name = "NSE:INFY")
    val nseInfy: NseInfy
)

data class NseInfy (
    @Json(name = "\$ref")
    val ref: String
)

data class Depth (
    val additionalProperties: Boolean,
    val properties: DepthProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DepthProperties (
    val buy: BuyClass,
    val sell: BuyClass
)

data class BuyClass (
    val items: NseInfy,
    val type: String
)

data class NseInfyClass (
    val additionalProperties: Boolean,
    val properties: NseInfyProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class NseInfyProperties (
    @Json(name = "average_price")
    val averagePrice: Orders,

    @Json(name = "buy_quantity")
    val buyQuantity: Orders,

    val depth: NseInfy,

    @Json(name = "instrument_token")
    val instrumentToken: Orders,

    @Json(name = "last_price")
    val lastPrice: Orders,

    @Json(name = "last_quantity")
    val lastQuantity: Orders,

    @Json(name = "last_trade_time")
    val lastTradeTime: LastTradeTime,

    @Json(name = "lower_circuit_limit")
    val lowerCircuitLimit: Orders,

    @Json(name = "net_change")
    val netChange: Orders,

    val ohlc: NseInfy,
    val oi: Orders,

    @Json(name = "oi_day_high")
    val oiDayHigh: Orders,

    @Json(name = "oi_day_low")
    val oiDayLow: Orders,

    @Json(name = "sell_quantity")
    val sellQuantity: Orders,

    val timestamp: LastTradeTime,

    @Json(name = "upper_circuit_limit")
    val upperCircuitLimit: Orders,

    val volume: Orders
)

data class LastTradeTime (
    val format: String,
    val type: Type
)

data class Ohlc (
    val additionalProperties: Boolean,
    val properties: OhlcProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OhlcProperties (
    val close: Orders,
    val high: Orders,
    val low: Orders,
    val open: Orders
)

data class QuoteClass (
    val additionalProperties: Boolean,
    val properties: QuoteProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class QuoteProperties (
    val data: NseInfy,
    val status: Orders
)
