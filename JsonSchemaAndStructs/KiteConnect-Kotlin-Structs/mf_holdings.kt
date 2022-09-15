// To parse the JSON, install Klaxon and do:
//
//   val mFHoldings = MFHoldings.fromJson(jsonString)

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

data class MFHoldings (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<MFHoldings>(json)
    }
}

data class Definitions (
    @Json(name = "Datum")
    val datum: Datum,

    @Json(name = "MFHoldings")
    val mfHoldings: MFHoldingsClass
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

    val folio: AveragePrice,
    val fund: AveragePrice,

    @Json(name = "last_price")
    val lastPrice: AveragePrice,

    @Json(name = "last_price_date")
    val lastPriceDate: AveragePrice,

    @Json(name = "pledged_quantity")
    val pledgedQuantity: AveragePrice,

    val pnl: AveragePrice,
    val quantity: AveragePrice,
    val tradingsymbol: AveragePrice
)

data class AveragePrice (
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

data class MFHoldingsClass (
    val additionalProperties: Boolean,
    val properties: MFHoldingsProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class MFHoldingsProperties (
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
