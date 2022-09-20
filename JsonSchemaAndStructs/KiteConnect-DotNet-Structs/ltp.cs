// <auto-generated />
//
// To parse this JSON data, add NuGet 'Newtonsoft.Json' then do:
//
//    using Ltp;
//
//    var ltp = Ltp.FromJson(jsonString);

namespace Ltp
{
    using System;
    using System.Collections.Generic;

    using System.Globalization;
    using Newtonsoft.Json;
    using Newtonsoft.Json.Converters;

    public partial class Ltp
    {
        [JsonProperty("data", NullValueHandling = NullValueHandling.Ignore)]
        public Dictionary<string, Datum> Data { get; set; }

        [JsonProperty("status", NullValueHandling = NullValueHandling.Ignore)]
        public string Status { get; set; }
    }

    public partial class Datum
    {
        [JsonProperty("instrument_token", NullValueHandling = NullValueHandling.Ignore)]
        public long? InstrumentToken { get; set; }

        [JsonProperty("last_price", NullValueHandling = NullValueHandling.Ignore)]
        public double? LastPrice { get; set; }
    }

    public partial class Ltp
    {
        public static Ltp FromJson(string json) => JsonConvert.DeserializeObject<Ltp>(json, Ltp.Converter.Settings);
    }

    public static class Serialize
    {
        public static string ToJson(this Ltp self) => JsonConvert.SerializeObject(self, Ltp.Converter.Settings);
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
