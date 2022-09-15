// To parse the JSON, install Klaxon and do:
//
//   val holdings = Holdings.fromJson(jsonString)

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

data class Holdings (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<Holdings>(json)
    }
}

data class Definitions (
    @Json(name = "Datum")
    val datum: Datum,

    @Json(name = "Holdings")
    val holdings: HoldingsClass
)

data class Datum (
    val additionalProperties: Boolean,
    val properties: DatumProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DatumProperties (
    @Json(name = "authorised_date")
    val authorisedDate: AuthorisedDate,

    @Json(name = "authorised_quantity")
    val authorisedQuantity: AuthorisedQuantity,

    @Json(name = "average_price")
    val averagePrice: AuthorisedQuantity,

    @Json(name = "close_price")
    val closePrice: AuthorisedQuantity,

    @Json(name = "collateral_quantity")
    val collateralQuantity: AuthorisedQuantity,

    @Json(name = "collateral_type")
    val collateralType: AuthorisedQuantity,

    @Json(name = "day_change")
    val dayChange: AuthorisedQuantity,

    @Json(name = "day_change_percentage")
    val dayChangePercentage: AuthorisedQuantity,

    val discrepancy: AuthorisedQuantity,
    val exchange: AuthorisedQuantity,

    @Json(name = "instrument_token")
    val instrumentToken: AuthorisedQuantity,

    val isin: AuthorisedQuantity,

    @Json(name = "last_price")
    val lastPrice: AuthorisedQuantity,

    @Json(name = "opening_quantity")
    val openingQuantity: AuthorisedQuantity,

    val pnl: AuthorisedQuantity,
    val price: AuthorisedQuantity,
    val product: AuthorisedQuantity,
    val quantity: AuthorisedQuantity,

    @Json(name = "realised_quantity")
    val realisedQuantity: AuthorisedQuantity,

    @Json(name = "t1_quantity")
    val t1Quantity: AuthorisedQuantity,

    val tradingsymbol: AuthorisedQuantity,

    @Json(name = "used_quantity")
    val usedQuantity: AuthorisedQuantity
)

data class AuthorisedDate (
    val format: String,
    val type: Type
)

enum class Type(val value: String) {
    Integer("integer"),
    Number("number"),
    TypeBoolean("boolean"),
    TypeString("string");

    companion object {
        public fun fromValue(value: String): Type = when (value) {
            "integer" -> Integer
            "number"  -> Number
            "boolean" -> TypeBoolean
            "string"  -> TypeString
            else      -> throw IllegalArgumentException()
        }
    }
}

data class AuthorisedQuantity (
    val type: Type
)

data class HoldingsClass (
    val additionalProperties: Boolean,
    val properties: HoldingsProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class HoldingsProperties (
    val data: Data,
    val status: AuthorisedQuantity
)

data class Data (
    val items: Items,
    val type: String
)

data class Items (
    @Json(name = "\$ref")
    val ref: String
)
