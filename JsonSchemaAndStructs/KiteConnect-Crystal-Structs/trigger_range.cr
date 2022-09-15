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

class TriggerRange
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

  @[JSON::Field(key: "Nse")]
  property nse : Nse

  @[JSON::Field(key: "TriggerRange")]
  property trigger_range : TriggerRangeClass
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

  @[JSON::Field(key: "NSE:RELIANCE")]
  property nse_reliance : NseInfy
end

class NseInfy
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class Nse
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : NseProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property nse_type : String
end

class NseProperties
  include JSON::Serializable

  property instrument_token : InstrumentToken

  property lower : InstrumentToken

  property upper : InstrumentToken
end

class InstrumentToken
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property instrument_token_type : String
end

class TriggerRangeClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : TriggerRangeProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property trigger_range_class_type : String
end

class TriggerRangeProperties
  include JSON::Serializable

  property data : NseInfy

  property status : InstrumentToken
end
