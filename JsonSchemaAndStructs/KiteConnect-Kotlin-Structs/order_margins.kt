// To parse the JSON, install Klaxon and do:
//
//   val orderMargins = OrderMargins.fromJson(jsonString)

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

data class OrderMargins (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<OrderMargins>(json)
    }
}

data class Definitions (
    @Json(name = "Datum")
    val datum: Datum,

    @Json(name = "OrderMargins")
    val orderMargins: OrderMarginsClass,

    @Json(name = "Pnl")
    val pnl: PnlClass
)

data class Datum (
    val additionalProperties: Boolean,
    val properties: DatumProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DatumProperties (
    val additional: Additional,
    val bo: Additional,
    val cash: Additional,
    val exchange: Additional,
    val exposure: Additional,

    @Json(name = "option_premium")
    val optionPremium: Additional,

    val pnl: Pnl,
    val span: Additional,
    val total: Additional,
    val tradingsymbol: Additional,
    val type: Additional,

    @Json(name = "var")
    val datumPropertiesVar: Additional
)

data class Additional (
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

data class Pnl (
    @Json(name = "\$ref")
    val ref: String
)

data class OrderMarginsClass (
    val additionalProperties: Boolean,
    val properties: OrderMarginsProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class OrderMarginsProperties (
    val data: Data,
    val status: Additional
)

data class Data (
    val items: Pnl,
    val type: String
)

data class PnlClass (
    val additionalProperties: Boolean,
    val properties: PnlProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class PnlProperties (
    val realised: Additional,
    val unrealised: Additional
)
