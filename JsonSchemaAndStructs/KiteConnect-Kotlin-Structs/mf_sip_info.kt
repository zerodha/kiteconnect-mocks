// To parse the JSON, install Klaxon and do:
//
//   val mFSIPInfo = MFSIPInfo.fromJson(jsonString)

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

data class MFSIPInfo (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<MFSIPInfo>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "MFSIPInfo")
    val mfsipInfo: MFSIPInfoClass,

    @Json(name = "StepUp")
    val stepUp: StepUpClass
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    @Json(name = "completed_instalments")
    val completedInstalments: CompletedInstalments,

    val created: Created,

    @Json(name = "dividend_type")
    val dividendType: CompletedInstalments,

    val frequency: CompletedInstalments,
    val fund: CompletedInstalments,

    @Json(name = "fund_source")
    val fundSource: CompletedInstalments,

    @Json(name = "instalment_amount")
    val instalmentAmount: CompletedInstalments,

    @Json(name = "instalment_day")
    val instalmentDay: CompletedInstalments,

    val instalments: CompletedInstalments,

    @Json(name = "last_instalment")
    val lastInstalment: Created,

    @Json(name = "next_instalment")
    val nextInstalment: Created,

    @Json(name = "pending_instalments")
    val pendingInstalments: CompletedInstalments,

    @Json(name = "sip_id")
    val sipID: CompletedInstalments,

    @Json(name = "sip_reg_num")
    val sipRegNum: CompletedInstalments,

    @Json(name = "sip_type")
    val sipType: CompletedInstalments,

    val status: CompletedInstalments,

    @Json(name = "step_up")
    val stepUp: StepUp,

    val tag: CompletedInstalments,
    val tradingsymbol: CompletedInstalments,

    @Json(name = "transaction_type")
    val transactionType: CompletedInstalments,

    @Json(name = "trigger_price")
    val triggerPrice: CompletedInstalments
)

data class CompletedInstalments (
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

data class Created (
    val format: String,
    val type: Type
)

data class StepUp (
    @Json(name = "\$ref")
    val ref: String
)

data class MFSIPInfoClass (
    val additionalProperties: Boolean,
    val properties: MFSIPInfoProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class MFSIPInfoProperties (
    val data: StepUp,
    val status: CompletedInstalments
)

data class StepUpClass (
    val additionalProperties: Boolean,
    val properties: StepUpProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class StepUpProperties (
    @Json(name = "15-02")
    val the1502: CompletedInstalments
)
