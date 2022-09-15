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

class HistoricalMinute
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Candle")]
  property candle : Candle

  @[JSON::Field(key: "Data")]
  property data : Data

  @[JSON::Field(key: "HistoricalMinute")]
  property historical_minute : HistoricalMinuteClass
end

class Candle
  include JSON::Serializable

  @[JSON::Field(key: "anyOf")]
  property any_of : Array(AnyOf)

  property title : String
end

class AnyOf
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property any_of_type : String
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

  property candles : Candles
end

class Candles
  include JSON::Serializable

  property items : Items

  @[JSON::Field(key: "type")]
  property candles_type : String
end

class Items
  include JSON::Serializable

  property items : DataClass

  @[JSON::Field(key: "type")]
  property items_type : String
end

class DataClass
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class HistoricalMinuteClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : HistoricalMinuteProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property historical_minute_class_type : String
end

class HistoricalMinuteProperties
  include JSON::Serializable

  property data : DataClass

  property status : AnyOf
end
