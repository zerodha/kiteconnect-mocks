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

class Quote
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Buy")]
  property buy : Buy

  @[JSON::Field(key: "Data")]
  property data : Data

  @[JSON::Field(key: "Depth")]
  property depth : Depth

  @[JSON::Field(key: "NseInfy")]
  property nse_infy : NseInfyClass

  @[JSON::Field(key: "Ohlc")]
  property ohlc : Ohlc

  @[JSON::Field(key: "Quote")]
  property quote : QuoteClass
end

class Buy
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : BuyProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property buy_type : String
end

class BuyProperties
  include JSON::Serializable

  property orders : Orders

  property price : Orders

  property quantity : Orders
end

class Orders
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property orders_type : String
end

class Data
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : DataProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property data_type : String
end

class DataProperties
  include JSON::Serializable

  @[JSON::Field(key: "NSE:INFY")]
  property nse_infy : NseInfy
end

class NseInfy
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class Depth
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : DepthProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property depth_type : String
end

class DepthProperties
  include JSON::Serializable

  property buy : BuyClass

  property sell : BuyClass
end

class BuyClass
  include JSON::Serializable

  property items : NseInfy

  @[JSON::Field(key: "type")]
  property buy_class_type : String
end

class NseInfyClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : NseInfyProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property nse_infy_class_type : String
end

class NseInfyProperties
  include JSON::Serializable

  property average_price : Orders

  property buy_quantity : Orders

  property depth : NseInfy

  property instrument_token : Orders

  property last_price : Orders

  property last_quantity : Orders

  property last_trade_time : LastTradeTime

  property lower_circuit_limit : Orders

  property net_change : Orders

  property ohlc : NseInfy

  property oi : Orders

  property oi_day_high : Orders

  property oi_day_low : Orders

  property sell_quantity : Orders

  property timestamp : LastTradeTime

  property upper_circuit_limit : Orders

  property volume : Orders
end

class LastTradeTime
  include JSON::Serializable

  property format : String

  @[JSON::Field(key: "type")]
  property last_trade_time_type : String
end

class Ohlc
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OhlcProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property ohlc_type : String
end

class OhlcProperties
  include JSON::Serializable

  property close : Orders

  property high : Orders

  property low : Orders

  property open : Orders
end

class QuoteClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : QuoteProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property quote_class_type : String
end

class QuoteProperties
  include JSON::Serializable

  property data : NseInfy

  property status : Orders
end
