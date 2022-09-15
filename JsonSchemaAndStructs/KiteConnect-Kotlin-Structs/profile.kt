// To parse the JSON, install Klaxon and do:
//
//   val profile = Profile.fromJson(jsonString)

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

data class Profile (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<Profile>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "Meta")
    val meta: MetaClass,

    @Json(name = "Profile")
    val profile: ProfileClass
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    @Json(name = "avatar_url")
    val avatarURL: AvatarURL,

    val broker: AvatarURL,
    val email: AvatarURL,
    val exchanges: Exchanges,
    val meta: Meta,

    @Json(name = "order_types")
    val orderTypes: Exchanges,

    val products: Exchanges,

    @Json(name = "user_id")
    val userID: AvatarURL,

    @Json(name = "user_name")
    val userName: AvatarURL,

    @Json(name = "user_shortname")
    val userShortname: AvatarURL,

    @Json(name = "user_type")
    val userType: AvatarURL
)

data class AvatarURL (
    val type: Type
)

enum class Type(val value: String) {
    Null("null"),
    TypeString("string");

    companion object {
        public fun fromValue(value: String): Type = when (value) {
            "null"   -> Null
            "string" -> TypeString
            else     -> throw IllegalArgumentException()
        }
    }
}

data class Exchanges (
    val items: AvatarURL,
    val type: String
)

data class Meta (
    @Json(name = "\$ref")
    val ref: String
)

data class MetaClass (
    val additionalProperties: Boolean,
    val properties: MetaProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class MetaProperties (
    @Json(name = "demat_consent")
    val dematConsent: AvatarURL
)

data class ProfileClass (
    val additionalProperties: Boolean,
    val properties: ProfileProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class ProfileProperties (
    val data: Meta,
    val status: AvatarURL
)
