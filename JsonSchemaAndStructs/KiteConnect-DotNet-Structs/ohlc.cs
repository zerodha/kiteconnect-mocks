// <auto-generated />
//
// To parse this JSON data, add NuGet 'Newtonsoft.Json' then do:
//
//    using QuickType;
//
//    var ohlc = Ohlc.FromJson(jsonString);

namespace QuickType
{
    using System;
    using System.Collections.Generic;

    using System.Globalization;
    using Newtonsoft.Json;
    using Newtonsoft.Json.Converters;

    public partial class Ohlc
    {
        [JsonProperty("$ref")]
        public string Ref { get; set; }

        [JsonProperty("$schema")]
        public Uri Schema { get; set; }

        [JsonProperty("definitions")]
        public Definitions Definitions { get; set; }
    }

    public partial class Definitions
    {
        [JsonProperty("Data")]
        public Data Data { get; set; }

        [JsonProperty("NseInfy")]
        public NseInfyClass NseInfy { get; set; }

        [JsonProperty("Ohlc")]
        public OhlcClass Ohlc { get; set; }

        [JsonProperty("OhlcClass")]
        public OhlcClassClass OhlcClass { get; set; }
    }

    public partial class Data
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public DataProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] DataRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class DataProperties
    {
        [JsonProperty("NSE:INFY")]
        public NseInfy NseInfy { get; set; }
    }

    public partial class NseInfy
    {
        [JsonProperty("$ref")]
        public string Ref { get; set; }
    }

    public partial class NseInfyClass
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public NseInfyProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] NseInfyClassRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class NseInfyProperties
    {
        [JsonProperty("instrument_token")]
        public InstrumentToken InstrumentToken { get; set; }

        [JsonProperty("last_price")]
        public InstrumentToken LastPrice { get; set; }

        [JsonProperty("ohlc")]
        public NseInfy Ohlc { get; set; }
    }

    public partial class InstrumentToken
    {
        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class OhlcClass
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public OhlcProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] OhlcClassRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class OhlcProperties
    {
        [JsonProperty("data")]
        public NseInfy Data { get; set; }

        [JsonProperty("status")]
        public InstrumentToken Status { get; set; }
    }

    public partial class OhlcClassClass
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public OhlcClassProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] OhlcClassClassRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class OhlcClassProperties
    {
        [JsonProperty("close")]
        public InstrumentToken Close { get; set; }

        [JsonProperty("high")]
        public InstrumentToken High { get; set; }

        [JsonProperty("low")]
        public InstrumentToken Low { get; set; }

        [JsonProperty("open")]
        public InstrumentToken Open { get; set; }
    }

    public partial class Ohlc
    {
        public static Ohlc FromJson(string json) => JsonConvert.DeserializeObject<Ohlc>(json, QuickType.Converter.Settings);
    }

    public static class Serialize
    {
        public static string ToJson(this Ohlc self) => JsonConvert.SerializeObject(self, QuickType.Converter.Settings);
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