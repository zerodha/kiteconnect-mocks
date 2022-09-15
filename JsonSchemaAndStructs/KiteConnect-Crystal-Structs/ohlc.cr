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

class Ohlc
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Data")]
  property data : Data

  @[JSON::Field(key: "NseInfy")]
  property nse_infy : NseInfyClass

  @[JSON::Field(key: "Ohlc")]
  property ohlc : OhlcClass

  @[JSON::Field(key: "OhlcClass")]
  property ohlc_class : OhlcClassClass
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

  property instrument_token : InstrumentToken

  property last_price : InstrumentToken

  property ohlc : NseInfy
end

class InstrumentToken
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property instrument_token_type : String
end

class OhlcClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OhlcProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property ohlc_class_type : String
end

class OhlcProperties
  include JSON::Serializable

  property data : NseInfy

  property status : InstrumentToken
end

class OhlcClassClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OhlcClassProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property ohlc_class_class_type : String
end

class OhlcClassProperties
  include JSON::Serializable

  property close : InstrumentToken

  property high : InstrumentToken

  property low : InstrumentToken

  property open : InstrumentToken
end
