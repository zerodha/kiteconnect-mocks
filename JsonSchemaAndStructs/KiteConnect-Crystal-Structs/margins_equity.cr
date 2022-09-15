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

class MarginsEquity
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Available")]
  property available : Available

  @[JSON::Field(key: "Data")]
  property data : Data

  @[JSON::Field(key: "MarginsEquity")]
  property margins_equity : MarginsEquityClass
end

class Available
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : AvailableProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property available_type : String
end

class AvailableProperties
  include JSON::Serializable

  property adhoc_margin : AdhocMargin

  property cash : AdhocMargin

  property collateral : AdhocMargin

  property intraday_payin : AdhocMargin

  property live_balance : AdhocMargin

  property opening_balance : AdhocMargin
end

class AdhocMargin
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property adhoc_margin_type : String
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

  property available : AvailableClass

  property enabled : AdhocMargin

  property net : AdhocMargin

  property utilised : Utilised
end

class AvailableClass
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class Utilised
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : AdhocMargin

  @[JSON::Field(key: "type")]
  property utilised_type : String
end

class MarginsEquityClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MarginsEquityProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property margins_equity_class_type : String
end

class MarginsEquityProperties
  include JSON::Serializable

  property data : AvailableClass

  property status : AdhocMargin
end
