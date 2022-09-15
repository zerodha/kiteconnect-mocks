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

class OrderResponse
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

  @[JSON::Field(key: "OrderResponse")]
  property order_response : OrderResponseClass
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

  @[JSON::Field(key: "type")]
  property order_id_type : String
end

class OrderResponseClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OrderResponseProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property order_response_class_type : String
end

class OrderResponseProperties
  include JSON::Serializable

  property data : DataClass

  property status : OrderId
end

class DataClass
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end
