// <auto-generated />
//
// To parse this JSON data, add NuGet 'Newtonsoft.Json' then do:
//
//    using OrderMargins;
//
//    var orderMargins = OrderMargins.FromJson(jsonString);

namespace OrderMargins
{
    using System;
    using System.Collections.Generic;

    using System.Globalization;
    using Newtonsoft.Json;
    using Newtonsoft.Json.Converters;

    public partial class OrderMargins
    {
        [JsonProperty("data", NullValueHandling = NullValueHandling.Ignore)]
        public Datum[] Data { get; set; }

        [JsonProperty("status", NullValueHandling = NullValueHandling.Ignore)]
        public string Status { get; set; }
    }

    public partial class Datum
    {
        [JsonProperty("additional", NullValueHandling = NullValueHandling.Ignore)]
        public long? Additional { get; set; }

        [JsonProperty("bo", NullValueHandling = NullValueHandling.Ignore)]
        public long? Bo { get; set; }

        [JsonProperty("cash", NullValueHandling = NullValueHandling.Ignore)]
        public long? Cash { get; set; }

        [JsonProperty("exchange", NullValueHandling = NullValueHandling.Ignore)]
        public string Exchange { get; set; }

        [JsonProperty("exposure", NullValueHandling = NullValueHandling.Ignore)]
        public long? Exposure { get; set; }

        [JsonProperty("option_premium", NullValueHandling = NullValueHandling.Ignore)]
        public long? OptionPremium { get; set; }

        [JsonProperty("pnl", NullValueHandling = NullValueHandling.Ignore)]
        public Pnl Pnl { get; set; }

        [JsonProperty("span", NullValueHandling = NullValueHandling.Ignore)]
        public long? Span { get; set; }

        [JsonProperty("total", NullValueHandling = NullValueHandling.Ignore)]
        public double? Total { get; set; }

        [JsonProperty("tradingsymbol", NullValueHandling = NullValueHandling.Ignore)]
        public string Tradingsymbol { get; set; }

        [JsonProperty("type", NullValueHandling = NullValueHandling.Ignore)]
        public string Type { get; set; }

        [JsonProperty("var", NullValueHandling = NullValueHandling.Ignore)]
        public double? Var { get; set; }
    }

    public partial class Pnl
    {
        [JsonProperty("realised", NullValueHandling = NullValueHandling.Ignore)]
        public long? Realised { get; set; }

        [JsonProperty("unrealised", NullValueHandling = NullValueHandling.Ignore)]
        public long? Unrealised { get; set; }
    }

    public partial class OrderMargins
    {
        public static OrderMargins FromJson(string json) => JsonConvert.DeserializeObject<OrderMargins>(json, OrderMargins.Converter.Settings);
    }

    public static class Serialize
    {
        public static string ToJson(this OrderMargins self) => JsonConvert.SerializeObject(self, OrderMargins.Converter.Settings);
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
