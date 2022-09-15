// To parse the JSON, install Klaxon and do:
//
//   val mFOrders = MFOrders.fromJson(jsonString)

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

data class MFOrders (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<MFOrders>(json)
    }
}

data class Definitions (
    @Json(name = "Datum")
    val datum: Datum,

    @Json(name = "MFOrders")
    val mfOrders: MFOrdersClass
)

data class Datum (
    val additionalProperties: Boolean,
    val properties: DatumProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DatumProperties (
    val amount: Amount,

    @Json(name = "average_price")
    val averagePrice: Amount,

    @Json(name = "exchange_order_id")
    val exchangeOrderID: ExchangeOrderID,

    @Json(name = "exchange_timestamp")
    val exchangeTimestamp: ExchangeOrderID,

    val folio: Amount,
    val fund: Amount,

    @Json(name = "last_price")
    val lastPrice: Amount,

    @Json(name = "last_price_date")
    val lastPriceDate: LastPriceDate,

    @Json(name = "order_id")
    val orderID: LastPriceDate,

    @Json(name = "order_timestamp")
    val orderTimestamp: LastPriceDate,

    @Json(name = "placed_by")
    val placedBy: Amount,

    @Json(name = "purchase_type")
    val purchaseType: Amount,

    val quantity: Amount,

    @Json(name = "settlement_id")
    val settlementID: ExchangeOrderID,

    val status: Amount,

    @Json(name = "status_message")
    val statusMessage: Amount,

    val tag: Tag,
    val tradingsymbol: Amount,

    @Json(name = "transaction_type")
    val transactionType: Amount,

    val variety: Amount
)

data class Amount (
    val type: Type
)

enum class Type(val value: String) {
    Null("null"),
    Number("number"),
    TypeString("string");

    companion object {
        public fun fromValue(value: String): Type = when (value) {
            "null"   -> Null
            "number" -> Number
            "string" -> TypeString
            else     -> throw IllegalArgumentException()
        }
    }
}

data class ExchangeOrderID (
    val anyOf: List<LastPriceDate>
)

data class LastPriceDate (
    val format: String? = null,
    val type: Type
)

data class Tag (
    val anyOf: List<Amount>
)

data class MFOrdersClass (
    val additionalProperties: Boolean,
    val properties: MFOrdersProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class MFOrdersProperties (
    val data: Data,
    val status: Amount
)

data class Data (
    val items: Items,
    val type: String
)

data class Items (
    @Json(name = "\$ref")
    val ref: String
)
