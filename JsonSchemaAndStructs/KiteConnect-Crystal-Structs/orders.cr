# Example code that deserializes and serializes the model:
#
# require "json"
#
# class Location
#   include JSON::Serializable
#
#   @[JSON::Field(key: "lat")]
#   property latitude : Float64
#
#   @[JSON::Field(key: "lng")]
#   property longitude : Float64
# end
#
# class House
#   include JSON::Serializable
#   property address : String
#   property location : Location?
# end
#
# house = House.from_json(%({"address": "Crystal Road 1234", "location": {"lat": 12.3, "lng": 34.5}}))
# house.address  # => "Crystal Road 1234"
# house.location # => #<Location:0x10cd93d80 @latitude=12.3, @longitude=34.5>


require "json"

class Orders
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Datum")]
  property datum : Datum

  @[JSON::Field(key: "Iceberg")]
  property iceberg : Iceberg

  @[JSON::Field(key: "Meta")]
  property meta : MetaClass

  @[JSON::Field(key: "Orders")]
  property orders : OrdersClass
end

class Datum
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : DatumProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property datum_type : String
end

class DatumProperties
  include JSON::Serializable

  property average_price : AveragePrice

  property cancelled_quantity : AveragePrice

  property disclosed_quantity : AveragePrice

  property exchange : AveragePrice

  property exchange_order_id : ExchangeOrderId

  property exchange_timestamp : ExchangeETimestamp

  property exchange_update_timestamp : ExchangeETimestamp

  property filled_quantity : AveragePrice

  property guid : AveragePrice

  property instrument_token : AveragePrice

  property market_protection : AveragePrice

  property meta : Meta

  property modified : AveragePrice

  property order_id : AveragePrice

  property order_timestamp : OrderTimestamp

  property order_type : AveragePrice

  property parent_order_id : AveragePrice

  property pending_quantity : AveragePrice

  property placed_by : AveragePrice

  property price : AveragePrice

  property product : AveragePrice

  property quantity : AveragePrice

  property status : AveragePrice

  property status_message : ExchangeOrderId

  property status_message_raw : ExchangeOrderId

  property tag : ExchangeOrderId

  property tags : Tags

  property tradingsymbol : AveragePrice

  property transaction_type : AveragePrice

  property trigger_price : AveragePrice

  property validity : AveragePrice

  property validity_ttl : AveragePrice

  property variety : AveragePrice
end

class AveragePrice
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property average_price_type : String
end

class ExchangeOrderId
  include JSON::Serializable

  @[JSON::Field(key: "anyOf")]
  property any_of : Array(AveragePrice)
end

class ExchangeETimestamp
  include JSON::Serializable

  @[JSON::Field(key: "anyOf")]
  property any_of : Array(OrderTimestamp)
end

class OrderTimestamp
  include JSON::Serializable

  property format : String?

  @[JSON::Field(key: "type")]
  property order_timestamp_type : String
end

class Meta
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class Tags
  include JSON::Serializable

  property items : AveragePrice

  @[JSON::Field(key: "type")]
  property tags_type : String
end

class Iceberg
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : IcebergProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property iceberg_type : String
end

class IcebergProperties
  include JSON::Serializable

  property leg : AveragePrice

  property leg_quantity : AveragePrice

  property legs : AveragePrice

  property remaining_quantity : AveragePrice

  property total_quantity : AveragePrice
end

class MetaClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MetaProperties

  property required : Array(JSON::Any?)

  property title : String

  @[JSON::Field(key: "type")]
  property meta_class_type : String
end

class MetaProperties
  include JSON::Serializable

  property iceberg : Meta
end

class OrdersClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OrdersProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property orders_class_type : String
end

class OrdersProperties
  include JSON::Serializable

  property data : Data

  property status : AveragePrice
end

class Data
  include JSON::Serializable

  property items : Meta

  @[JSON::Field(key: "type")]
  property data_type : String
end
