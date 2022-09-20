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

  property data : Array(Datum)?

  property status : String?
end

class Datum
  include JSON::Serializable

  property additional : Int32?

  property bo : Int32?

  property cash : Int32?

  property exchange : String?

  property exposure : Int32?

  property option_premium : Int32?

  property pnl : Pnl?

  property span : Int32?

  property total : Float64?

  property tradingsymbol : String?

  @[JSON::Field(key: "type")]
  property datum_type : String?

  property var : Float64?
end

class Pnl
  include JSON::Serializable

  property realised : Int32?

  property unrealised : Int32?
end
