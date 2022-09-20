// <auto-generated />
//
// To parse this JSON data, add NuGet 'Newtonsoft.Json' then do:
//
//    using GttGetOrder;
//
//    var gttGetOrder = GttGetOrder.FromJson(jsonString);

namespace GttGetOrder
{
    using System;
    using System.Collections.Generic;

    using System.Globalization;
    using Newtonsoft.Json;
    using Newtonsoft.Json.Converters;

    public partial class GttGetOrder
    {
        [JsonProperty("data", NullValueHandling = NullValueHandling.Ignore)]
        public Data Data { get; set; }

        [JsonProperty("status", NullValueHandling = NullValueHandling.Ignore)]
        public string Status { get; set; }
    }

    public partial class Data
    {
        [JsonProperty("condition", NullValueHandling = NullValueHandling.Ignore)]
        public Condition Condition { get; set; }

        [JsonProperty("created_at", NullValueHandling = NullValueHandling.Ignore)]
        public DateTimeOffset? CreatedAt { get; set; }

        [JsonProperty("expires_at", NullValueHandling = NullValueHandling.Ignore)]
        public DateTimeOffset? ExpiresAt { get; set; }

        [JsonProperty("id", NullValueHandling = NullValueHandling.Ignore)]
        public long? Id { get; set; }

        [JsonProperty("meta")]
        public object Meta { get; set; }

        [JsonProperty("orders", NullValueHandling = NullValueHandling.Ignore)]
        public Order[] Orders { get; set; }

        [JsonProperty("parent_trigger")]
        public object ParentTrigger { get; set; }

        [JsonProperty("status", NullValueHandling = NullValueHandling.Ignore)]
        public string Status { get; set; }

        [JsonProperty("type", NullValueHandling = NullValueHandling.Ignore)]
        public string Type { get; set; }

        [JsonProperty("updated_at", NullValueHandling = NullValueHandling.Ignore)]
        public DateTimeOffset? UpdatedAt { get; set; }

        [JsonProperty("user_id", NullValueHandling = NullValueHandling.Ignore)]
        public string UserId { get; set; }
    }

    public partial class Condition
    {
        [JsonProperty("exchange", NullValueHandling = NullValueHandling.Ignore)]
        public string Exchange { get; set; }

        [JsonProperty("instrument_token", NullValueHandling = NullValueHandling.Ignore)]
        public long? InstrumentToken { get; set; }

        [JsonProperty("last_price", NullValueHandling = NullValueHandling.Ignore)]
        public double? LastPrice { get; set; }

        [JsonProperty("tradingsymbol", NullValueHandling = NullValueHandling.Ignore)]
        public string Tradingsymbol { get; set; }

        [JsonProperty("trigger_values", NullValueHandling = NullValueHandling.Ignore)]
        public double[] TriggerValues { get; set; }
    }

    public partial class Order
    {
        [JsonProperty("exchange", NullValueHandling = NullValueHandling.Ignore)]
        public string Exchange { get; set; }

        [JsonProperty("order_type", NullValueHandling = NullValueHandling.Ignore)]
        public string OrderType { get; set; }

        [JsonProperty("price", NullValueHandling = NullValueHandling.Ignore)]
        public long? Price { get; set; }

        [JsonProperty("product", NullValueHandling = NullValueHandling.Ignore)]
        public string Product { get; set; }

        [JsonProperty("quantity", NullValueHandling = NullValueHandling.Ignore)]
        public long? Quantity { get; set; }

        [JsonProperty("result")]
        public Result Result { get; set; }

        [JsonProperty("tradingsymbol", NullValueHandling = NullValueHandling.Ignore)]
        public string Tradingsymbol { get; set; }

        [JsonProperty("transaction_type", NullValueHandling = NullValueHandling.Ignore)]
        public string TransactionType { get; set; }
    }

    public partial class Result
    {
        [JsonProperty("account_id", NullValueHandling = NullValueHandling.Ignore)]
        public string AccountId { get; set; }

        [JsonProperty("exchange", NullValueHandling = NullValueHandling.Ignore)]
        public string Exchange { get; set; }

        [JsonProperty("meta", NullValueHandling = NullValueHandling.Ignore)]
        public string Meta { get; set; }

        [JsonProperty("order_result", NullValueHandling = NullValueHandling.Ignore)]
        public OrderResult OrderResult { get; set; }

        [JsonProperty("order_type", NullValueHandling = NullValueHandling.Ignore)]
        public string OrderType { get; set; }

        [JsonProperty("price", NullValueHandling = NullValueHandling.Ignore)]
        public long? Price { get; set; }

        [JsonProperty("product", NullValueHandling = NullValueHandling.Ignore)]
        public string Product { get; set; }

        [JsonProperty("quantity", NullValueHandling = NullValueHandling.Ignore)]
        public long? Quantity { get; set; }

        [JsonProperty("timestamp", NullValueHandling = NullValueHandling.Ignore)]
        public DateTimeOffset? Timestamp { get; set; }

        [JsonProperty("tradingsymbol", NullValueHandling = NullValueHandling.Ignore)]
        public string Tradingsymbol { get; set; }

        [JsonProperty("transaction_type", NullValueHandling = NullValueHandling.Ignore)]
        public string TransactionType { get; set; }

        [JsonProperty("triggered_at", NullValueHandling = NullValueHandling.Ignore)]
        public double? TriggeredAt { get; set; }

        [JsonProperty("validity", NullValueHandling = NullValueHandling.Ignore)]
        public string Validity { get; set; }
    }

    public partial class OrderResult
    {
        [JsonProperty("order_id", NullValueHandling = NullValueHandling.Ignore)]
        public string OrderId { get; set; }

        [JsonProperty("rejection_reason", NullValueHandling = NullValueHandling.Ignore)]
        public string RejectionReason { get; set; }

        [JsonProperty("status", NullValueHandling = NullValueHandling.Ignore)]
        public string Status { get; set; }
    }

    public partial class GttGetOrder
    {
        public static GttGetOrder FromJson(string json) => JsonConvert.DeserializeObject<GttGetOrder>(json, GttGetOrder.Converter.Settings);
    }

    public static class Serialize
    {
        public static string ToJson(this GttGetOrder self) => JsonConvert.SerializeObject(self, GttGetOrder.Converter.Settings);
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
