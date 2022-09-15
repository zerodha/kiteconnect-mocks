// To parse the JSON, install Klaxon and do:
//
//   val gttGetOrders = GttGetOrders.fromJson(jsonString)

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

data class GttGetOrders (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<GttGetOrders>(json)
    }
}

data class Definitions (
    @Json(name = "Condition")
    val condition: Condition,

    @Json(name = "Datum")
    val datum: Datum,

    @Json(name = "GttGetOrders")
    val gttGetOrders: GttGetOrdersClass,

    @Json(name = "Meta")
    val meta: MetaClass,

    @Json(name = "Order")
    val order: Order,

    @Json(name = "OrderResult")
    val orderResult: OrderResult,

    @Json(name = "Result")
    val result: Result
)

data class Condition (
    val additionalProperties: Boolean,
    val properties: ConditionProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class ConditionProperties (
    val exchange: Exchange,

    @Json(name = "instrument_token")
    val instrumentToken: Exchange,

    @Json(name = "last_price")
    val lastPrice: Exchange,

    val tradingsymbol: Exchange,

    @Json(name = "trigger_values")
    val triggerValues: TriggerValues
)

data class Exchange (
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

data class TriggerValues (
    val items: Exchange,
    val type: String
)

data class Datum (
    val additionalProperties: Boolean,
    val properties: DatumProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DatumProperties (
    val condition: ConditionClass,

    @Json(name = "created_at")
    val createdAt: CreatedAt,

    @Json(name = "expires_at")
    val expiresAt: CreatedAt,

    val id: Exchange,
    val meta: Meta,
    val orders: Orders,

    @Json(name = "parent_trigger")
    val parentTrigger: Exchange,

    val status: Exchange,
    val type: Exchange,

    @Json(name = "updated_at")
    val updatedAt: CreatedAt,

    @Json(name = "user_id")
    val userID: Exchange
)

data class ConditionClass (
    @Json(name = "\$ref")
    val ref: String
)

data class CreatedAt (
    val format: String,
    val type: Type
)

data class Meta (
    val anyOf: List<AnyOf>
)

data class AnyOf (
    @Json(name = "\$ref")
    val ref: String? = null,

    val type: Type? = null
)

data class Orders (
    val items: ConditionClass,
    val type: String
)

data class GttGetOrdersClass (
    val additionalProperties: Boolean,
    val properties: GttGetOrdersProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class GttGetOrdersProperties (
    val data: Orders,
    val status: Exchange
)

data class MetaClass (
    val additionalProperties: Boolean,
    val title: String,
    val type: String
)

data class Order (
    val additionalProperties: Boolean,
    val properties: OrderProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OrderProperties (
    val exchange: Exchange,

    @Json(name = "order_type")
    val orderType: Exchange,

    val price: Exchange,
    val product: Exchange,
    val quantity: Exchange,
    val result: Meta,
    val tradingsymbol: Exchange,

    @Json(name = "transaction_type")
    val transactionType: Exchange
)

data class OrderResult (
    val additionalProperties: Boolean,
    val properties: OrderResultProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OrderResultProperties (
    @Json(name = "order_id")
    val orderID: Exchange,

    @Json(name = "rejection_reason")
    val rejectionReason: Exchange,

    val status: Exchange
)

data class Result (
    val additionalProperties: Boolean,
    val properties: ResultProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class ResultProperties (
    @Json(name = "account_id")
    val accountID: Exchange,

    val exchange: Exchange,
    val meta: Exchange,

    @Json(name = "order_result")
    val orderResult: ConditionClass,

    @Json(name = "order_type")
    val orderType: Exchange,

    val price: Exchange,
    val product: Exchange,
    val quantity: Exchange,
    val timestamp: CreatedAt,
    val tradingsymbol: Exchange,

    @Json(name = "transaction_type")
    val transactionType: Exchange,

    @Json(name = "triggered_at")
    val triggeredAt: Exchange,

    val validity: Exchange
)
