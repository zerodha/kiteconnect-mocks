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

class OrderInfo
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

  @[JSON::Field(key: "OrderInfo")]
  property order_info : OrderInfoClass
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

  property exchange_timestamp : ExchangeTimestamp

  property filled_quantity : AveragePrice

  property instrument_token : AveragePrice

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

  property status_message : AveragePrice

  property tag : AveragePrice

  property tradingsymbol : AveragePrice

  property transaction_type : AveragePrice

  property trigger_price : AveragePrice

  property validity : AveragePrice

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

class ExchangeTimestamp
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

class OrderInfoClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OrderInfoProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property order_info_class_type : String
end

class OrderInfoProperties
  include JSON::Serializable

  property data : Data

  property status : AveragePrice
end

class Data
  include JSON::Serializable

  property items : Items

  @[JSON::Field(key: "type")]
  property data_type : String
end

class Items
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end
