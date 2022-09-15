// To parse the JSON, install Klaxon and do:
//
//   val postback = Postback.fromJson(jsonString)

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

data class Postback (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<Postback>(json)
    }
}

data class Definitions (
    @Json(name = "Meta")
    val meta: Meta,

    @Json(name = "Postback")
    val postback: PostbackClass
)

data class Meta (
    val additionalProperties: Boolean,
    val title: String,
    val type: String
)

data class PostbackClass (
    val additionalProperties: Boolean,
    val properties: Properties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class Properties (
    @Json(name = "app_id")
    val appID: AppID,

    @Json(name = "average_price")
    val averagePrice: AppID,

    @Json(name = "cancelled_quantity")
    val cancelledQuantity: AppID,

    val checksum: AppID,

    @Json(name = "disclosed_quantity")
    val disclosedQuantity: AppID,

    val exchange: AppID,

    @Json(name = "exchange_order_id")
    val exchangeOrderID: AppID,

    @Json(name = "exchange_timestamp")
    val exchangeTimestamp: Timestamp,

    @Json(name = "exchange_update_timestamp")
    val exchangeUpdateTimestamp: Timestamp,

    @Json(name = "filled_quantity")
    val filledQuantity: AppID,

    val guid: AppID,

    @Json(name = "instrument_token")
    val instrumentToken: AppID,

    @Json(name = "market_protection")
    val marketProtection: AppID,

    val meta: MetaClass,

    @Json(name = "order_id")
    val orderID: AppID,

    @Json(name = "order_timestamp")
    val orderTimestamp: Timestamp,

    @Json(name = "order_type")
    val orderType: AppID,

    @Json(name = "parent_order_id")
    val parentOrderID: AppID,

    @Json(name = "pending_quantity")
    val pendingQuantity: AppID,

    @Json(name = "placed_by")
    val placedBy: AppID,

    val price: AppID,
    val product: AppID,
    val quantity: AppID,
    val status: AppID,

    @Json(name = "status_message")
    val statusMessage: AppID,

    @Json(name = "status_message_raw")
    val statusMessageRaw: AppID,

    val tag: AppID,
    val tradingsymbol: AppID,

    @Json(name = "transaction_type")
    val transactionType: AppID,

    @Json(name = "trigger_price")
    val triggerPrice: AppID,

    @Json(name = "unfilled_quantity")
    val unfilledQuantity: AppID,

    @Json(name = "user_id")
    val userID: AppID,

    val validity: AppID,
    val variety: AppID
)

data class AppID (
    val type: Type
)

enum class Type(val value: String) {
    Integer("integer"),
    Null("null"),
    TypeString("string");

    companion object {
        public fun fromValue(value: String): Type = when (value) {
            "integer" -> Integer
            "null"    -> Null
            "string"  -> TypeString
            else      -> throw IllegalArgumentException()
        }
    }
}

data class Timestamp (
    val format: String,
    val type: Type
)

data class MetaClass (
    @Json(name = "\$ref")
    val ref: String
)
