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

class OrderMargins
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

  @[JSON::Field(key: "OrderMargins")]
  property order_margins : OrderMarginsClass

  @[JSON::Field(key: "Pnl")]
  property pnl : PnlClass
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

  property additional : Additional

  property bo : Additional

  property cash : Additional

  property exchange : Additional

  property exposure : Additional

  property option_premium : Additional

  property pnl : Pnl

  property span : Additional

  property total : Additional

  property tradingsymbol : Additional

  @[JSON::Field(key: "type")]
  property datum_properties_type : Additional

  property var : Additional
end

class Additional
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property additional_type : String
end

class Pnl
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class OrderMarginsClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OrderMarginsProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property order_margins_class_type : String
end

class OrderMarginsProperties
  include JSON::Serializable

  property data : Data

  property status : Additional
end

class Data
  include JSON::Serializable

  property items : Pnl

  @[JSON::Field(key: "type")]
  property data_type : String
end

class PnlClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : PnlProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property pnl_class_type : String
end

class PnlProperties
  include JSON::Serializable

  property realised : Additional

  property unrealised : Additional
end
