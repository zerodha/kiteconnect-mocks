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

class MfOrderCancel
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

  @[JSON::Field(key: "MFOrderCancel")]
  property mf_order_cancel : MfOrderCancelClass
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

  property order_id : OrderId
end

class OrderId
  include JSON::Serializable

  property format : String

  @[JSON::Field(key: "type")]
  property order_id_type : String
end

class MfOrderCancelClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MfOrderCancelProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property mf_order_cancel_class_type : String
end

class MfOrderCancelProperties
  include JSON::Serializable

  property data : DataClass

  property status : Status
end

class DataClass
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class Status
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property status_type : String
end
