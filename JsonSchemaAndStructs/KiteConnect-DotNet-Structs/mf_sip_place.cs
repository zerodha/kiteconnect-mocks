// <auto-generated />
//
// To parse this JSON data, add NuGet 'Newtonsoft.Json' then do:
//
//    using MfSipPlace;
//
//    var mfSipPlace = MfSipPlace.FromJson(jsonString);

namespace MfSipPlace
{
    using System;
    using System.Collections.Generic;

    using System.Globalization;
    using Newtonsoft.Json;
    using Newtonsoft.Json.Converters;

    public partial class MfSipPlace
    {
        [JsonProperty("data", NullValueHandling = NullValueHandling.Ignore)]
        public Data Data { get; set; }

        [JsonProperty("status", NullValueHandling = NullValueHandling.Ignore)]
        public string Status { get; set; }
    }

    public partial class Data
    {
        [JsonProperty("sip_id", NullValueHandling = NullValueHandling.Ignore)]
        public string SipId { get; set; }
    }

    public partial class MfSipPlace
    {
        public static MfSipPlace FromJson(string json) => JsonConvert.DeserializeObject<MfSipPlace>(json, MfSipPlace.Converter.Settings);
    }

    public static class Serialize
    {
        public static string ToJson(this MfSipPlace self) => JsonConvert.SerializeObject(self, MfSipPlace.Converter.Settings);
    }

    internal static class Converter
    {
        public static readonly JsonSerializerSettings Settings = new JsonSerializerSettings
        {
            MetadataPropertyHandling = MetadataPropertyHandling.Ignore,
            DateParseHandling = DateParseHandling.None,
            Converters =
            {
                new IsoDateTimeConverter { DateTimeStyles = DateTimeStyles.AssumeUniversal }
            },
        };
    }
}
