// To parse the JSON, install Klaxon and do:
//
//   val mFSIPCancel = MFSIPCancel.fromJson(jsonString)

package quicktype

import com.beust.klaxon.*

private val klaxon = Klaxon()

data class MFSIPCancel (
    @Json(name = "\$ref")
    val ref: String,

    @Json(name = "\$schema")
    val schema: String,

    val definitions: Definitions
) {
    public fun toJson() = klaxon.toJsonString(this)

    companion object {
        public fun fromJson(json: String) = klaxon.parse<MFSIPCancel>(json)
    }
}

data class Definitions (
    @Json(name = "Data")
    val data: Data,

    @Json(name = "MFSIPCancel")
    val mfsipCancel: MFSIPCancelClass
)

data class Data (
    val additionalProperties: Boolean,
    val properties: DataProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class DataProperties (
    @Json(name = "sip_id")
    val sipID: Sipid
)

data class Sipid (
    val type: String
)

data class MFSIPCancelClass (
    val additionalProperties: Boolean,
    val properties: MFSIPCancelProperties,
    val required: List<String>,
    val title: String,
    val type: String
)

data class MFSIPCancelProperties (
    val data: DataClass,
    val status: Sipid
)

data class DataClass (
    @Json(name = "\$ref")
    val ref: String
)
