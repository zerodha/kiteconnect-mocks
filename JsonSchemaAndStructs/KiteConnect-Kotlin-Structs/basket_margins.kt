// To parse the JSON, install Klaxon and do:
//
//   val basketMargins = BasketMargins.fromJson(jsonString)

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

data class BasketMargins (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<BasketMargins>(json)
    }
}

data class Definitions (
    @Json(name = "BasketMargins")
    val basketMargins: BasketMarginsClass,

    @Json(name = "Data")
    val data: DataClass,

    @Json(name = "Final")
    val final: Final,

    @Json(name = "Pnl")
    val pnl: Pnl
)

data class BasketMarginsClass (
    val additionalProperties: Boolean,
    val properties: BasketMarginsProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class BasketMarginsProperties (
    val data: Data,
    val status: Status
)

data class Data (
    @Json(name = "\$ref")
    val ref: String
)

data class Status (
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

data class DataClass (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    val final: Data,
    val initial: Data,
    val orders: Orders
)

data class Orders (
    val items: Data,
    val type: String
)

data class Final (
    val additionalProperties: Boolean,
    val properties: FinalProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class FinalProperties (
    val additional: Status,
    val bo: Status,
    val cash: Status,
    val exchange: Status,
    val exposure: Status,

    @Json(name = "option_premium")
    val optionPremium: Status,

    val pnl: Data,
    val span: Status,
    val total: Status,
    val tradingsymbol: Status,
    val type: Status,

    @Json(name = "var")
    val finalPropertiesVar: Status
)

data class Pnl (
    val additionalProperties: Boolean,
    val properties: PnlProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class PnlProperties (
    val realised: Status,
    val unrealised: Status
)
