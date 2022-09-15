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

class BasketMargins
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "BasketMargins")]
  property basket_margins : BasketMarginsClass

  @[JSON::Field(key: "Data")]
  property data : DataClass

  @[JSON::Field(key: "Final")]
  property final : Final

  @[JSON::Field(key: "Pnl")]
  property pnl : Pnl
end

class BasketMarginsClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : BasketMarginsProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property basket_margins_class_type : String
end

class BasketMarginsProperties
  include JSON::Serializable

  property data : Data

  property status : Status
end

class Data
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class Status
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property status_type : String
end

class DataClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : DataProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property data_class_type : String
end

class DataProperties
  include JSON::Serializable

  property final : Data

  property initial : Data

  property orders : Orders
end

class Orders
  include JSON::Serializable

  property items : Data

  @[JSON::Field(key: "type")]
  property orders_type : String
end

class Final
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : FinalProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property final_type : String
end

class FinalProperties
  include JSON::Serializable

  property additional : Status

  property bo : Status

  property cash : Status

  property exchange : Status

  property exposure : Status

  property option_premium : Status

  property pnl : Data

  property span : Status

  property total : Status

  property tradingsymbol : Status

  @[JSON::Field(key: "type")]
  property final_properties_type : Status

  property var : Status
end

class Pnl
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : PnlProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property pnl_type : String
end

class PnlProperties
  include JSON::Serializable

  property realised : Status

  property unrealised : Status
end
