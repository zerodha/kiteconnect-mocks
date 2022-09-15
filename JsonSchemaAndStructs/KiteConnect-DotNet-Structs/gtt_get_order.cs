// <auto-generated />
//
// To parse this JSON data, add NuGet 'Newtonsoft.Json' then do:
//
//    using QuickType;
//
//    var gttGetOrder = GttGetOrder.FromJson(jsonString);

namespace QuickType
{
    using System;
    using System.Collections.Generic;

    using System.Globalization;
    using Newtonsoft.Json;
    using Newtonsoft.Json.Converters;

    public partial class GttGetOrder
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
        [JsonProperty("Condition")]
        public Condition Condition { get; set; }

        [JsonProperty("Data")]
        public Data Data { get; set; }

        [JsonProperty("GttGetOrder")]
        public GttGetOrderClass GttGetOrder { get; set; }

        [JsonProperty("Order")]
        public Order Order { get; set; }

        [JsonProperty("OrderResult")]
        public OrderResult OrderResult { get; set; }

        [JsonProperty("Result")]
        public ResultClass Result { get; set; }
    }

    public partial class Condition
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public ConditionProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] ConditionRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class ConditionProperties
    {
        [JsonProperty("exchange")]
        public Exchange Exchange { get; set; }

        [JsonProperty("instrument_token")]
        public Exchange InstrumentToken { get; set; }

        [JsonProperty("last_price")]
        public Exchange LastPrice { get; set; }

        [JsonProperty("tradingsymbol")]
        public Exchange Tradingsymbol { get; set; }

        [JsonProperty("trigger_values")]
        public TriggerValues TriggerValues { get; set; }
    }

    public partial class Exchange
    {
        [JsonProperty("type")]
        public TypeEnum Type { get; set; }
    }

    public partial class TriggerValues
    {
        [JsonProperty("items")]
        public Exchange Items { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
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
        [JsonProperty("condition")]
        public ConditionClass Condition { get; set; }

        [JsonProperty("created_at")]
        public CreatedAt CreatedAt { get; set; }

        [JsonProperty("expires_at")]
        public CreatedAt ExpiresAt { get; set; }

        [JsonProperty("id")]
        public Exchange Id { get; set; }

        [JsonProperty("meta")]
        public Exchange Meta { get; set; }

        [JsonProperty("orders")]
        public Orders Orders { get; set; }

        [JsonProperty("parent_trigger")]
        public Exchange ParentTrigger { get; set; }

        [JsonProperty("status")]
        public Exchange Status { get; set; }

        [JsonProperty("type")]
        public Exchange Type { get; set; }

        [JsonProperty("updated_at")]
        public CreatedAt UpdatedAt { get; set; }

        [JsonProperty("user_id")]
        public Exchange UserId { get; set; }
    }

    public partial class ConditionClass
    {
        [JsonProperty("$ref")]
        public string Ref { get; set; }
    }

    public partial class CreatedAt
    {
        [JsonProperty("format")]
        public string Format { get; set; }

        [JsonProperty("type")]
        public TypeEnum Type { get; set; }
    }

    public partial class Orders
    {
        [JsonProperty("items")]
        public ConditionClass Items { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class GttGetOrderClass
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public GttGetOrderProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] GttGetOrderClassRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class GttGetOrderProperties
    {
        [JsonProperty("data")]
        public ConditionClass Data { get; set; }

        [JsonProperty("status")]
        public Exchange Status { get; set; }
    }

    public partial class Order
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public OrderProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] OrderRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class OrderProperties
    {
        [JsonProperty("exchange")]
        public Exchange Exchange { get; set; }

        [JsonProperty("order_type")]
        public Exchange OrderType { get; set; }

        [JsonProperty("price")]
        public Exchange Price { get; set; }

        [JsonProperty("product")]
        public Exchange Product { get; set; }

        [JsonProperty("quantity")]
        public Exchange Quantity { get; set; }

        [JsonProperty("result")]
        public Result Result { get; set; }

        [JsonProperty("tradingsymbol")]
        public Exchange Tradingsymbol { get; set; }

        [JsonProperty("transaction_type")]
        public Exchange TransactionType { get; set; }
    }

    public partial class Result
    {
        [JsonProperty("anyOf")]
        public AnyOf[] AnyOf { get; set; }
    }

    public partial class AnyOf
    {
        [JsonProperty("$ref", NullValueHandling = NullValueHandling.Ignore)]
        public string Ref { get; set; }

        [JsonProperty("type", NullValueHandling = NullValueHandling.Ignore)]
        public TypeEnum? Type { get; set; }
    }

    public partial class OrderResult
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public OrderResultProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] OrderResultRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class OrderResultProperties
    {
        [JsonProperty("order_id")]
        public Exchange OrderId { get; set; }

        [JsonProperty("rejection_reason")]
        public Exchange RejectionReason { get; set; }

        [JsonProperty("status")]
        public Exchange Status { get; set; }
    }

    public partial class ResultClass
    {
        [JsonProperty("additionalProperties")]
        public bool AdditionalProperties { get; set; }

        [JsonProperty("properties")]
        public ResultProperties Properties { get; set; }

        [JsonProperty("required")]
        public string[] ResultClassRequired { get; set; }

        [JsonProperty("title")]
        public string Title { get; set; }

        [JsonProperty("type")]
        public string Type { get; set; }
    }

    public partial class ResultProperties
    {
        [JsonProperty("account_id")]
        public Exchange AccountId { get; set; }

        [JsonProperty("exchange")]
        public Exchange Exchange { get; set; }

        [JsonProperty("meta")]
        public Exchange Meta { get; set; }

        [JsonProperty("order_result")]
        public ConditionClass OrderResult { get; set; }

        [JsonProperty("order_type")]
        public Exchange OrderType { get; set; }

        [JsonProperty("price")]
        public Exchange Price { get; set; }

        [JsonProperty("product")]
        public Exchange Product { get; set; }

        [JsonProperty("quantity")]
        public Exchange Quantity { get; set; }

        [JsonProperty("timestamp")]
        public CreatedAt Timestamp { get; set; }

        [JsonProperty("tradingsymbol")]
        public Exchange Tradingsymbol { get; set; }

        [JsonProperty("transaction_type")]
        public Exchange TransactionType { get; set; }

        [JsonProperty("triggered_at")]
        public Exchange TriggeredAt { get; set; }

        [JsonProperty("validity")]
        public Exchange Validity { get; set; }
    }

    public enum TypeEnum { Integer, Null, Number, String };

    public partial class GttGetOrder
    {
        public static GttGetOrder FromJson(string json) => JsonConvert.DeserializeObject<GttGetOrder>(json, QuickType.Converter.Settings);
    }

    public static class Serialize
    {
        public static string ToJson(this GttGetOrder self) => JsonConvert.SerializeObject(self, QuickType.Converter.Settings);
    }

    internal static class Converter
    {
        public static readonly JsonSerializerSettings Settings = new JsonSerializerSettings
        {
            MetadataPropertyHandling = MetadataPropertyHandling.Ignore,
            DateParseHandling = DateParseHandling.None,
            Converters =
            {
                TypeEnumConverter.Singleton,
                new IsoDateTimeConverter { DateTimeStyles = DateTimeStyles.AssumeUniversal }
            },
        };
    }

    internal class TypeEnumConverter : JsonConverter
    {
        public override bool CanConvert(Type t) => t == typeof(TypeEnum) || t == typeof(TypeEnum?);

        public override object ReadJson(JsonReader reader, Type t, object existingValue, JsonSerializer serializer)
        {
            if (reader.TokenType == JsonToken.Null) return null;
            var value = serializer.Deserialize<string>(reader);
            switch (value)
            {
                case "integer":
                    return TypeEnum.Integer;
                case "null":
                    return TypeEnum.Null;
                case "number":
                    return TypeEnum.Number;
                case "string":
                    return TypeEnum.String;
            }
            throw new Exception("Cannot unmarshal type TypeEnum");
        }

        public override void WriteJson(JsonWriter writer, object untypedValue, JsonSerializer serializer)
        {
            if (untypedValue == null)
            {
                serializer.Serialize(writer, null);
                return;
            }
            var value = (TypeEnum)untypedValue;
            switch (value)
            {
                case TypeEnum.Integer:
                    serializer.Serialize(writer, "integer");
                    return;
                case TypeEnum.Null:
                    serializer.Serialize(writer, "null");
                    return;
                case TypeEnum.Number:
                    serializer.Serialize(writer, "number");
                    return;
                case TypeEnum.String:
                    serializer.Serialize(writer, "string");
                    return;
            }
            throw new Exception("Cannot marshal type TypeEnum");
        }

        public static readonly TypeEnumConverter Singleton = new TypeEnumConverter();
    }
}