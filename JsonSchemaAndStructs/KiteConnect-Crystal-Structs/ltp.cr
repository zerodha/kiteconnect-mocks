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

class Ltp
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

  @[JSON::Field(key: "Ltp")]
  property ltp : LtpClass

  @[JSON::Field(key: "NseInfy")]
  property nse_infy : NseInfyClass
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

class LtpClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : LtpProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property ltp_class_type : String
end

class LtpProperties
  include JSON::Serializable

  property data : NseInfy

  property status : Status
end

class Status
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property status_type : String
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

  property instrument_token : Status

  property last_price : Status
end
