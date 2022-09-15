// To parse the JSON, install Klaxon and do:
//
//   val positions = Positions.fromJson(jsonString)

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

data class Positions (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<Positions>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "Day")
    val day: DayClass,

    @Json(name = "Positions")
    val positions: PositionsClass
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    val day: Day,
    val net: Day
)

data class Day (
    val items: DataClass,
    val type: String
)

data class DataClass (
    @Json(name = "\$ref")
    val ref: String
)

data class DayClass (
    val additionalProperties: Boolean,
    val properties: Map<String, Property>,
    val required: List<String>,
    val title: String,
    val type: String
)

data class Property (
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

data class PositionsClass (
    val additionalProperties: Boolean,
    val properties: PositionsProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class PositionsProperties (
    val data: DataClass,
    val status: Property
)
