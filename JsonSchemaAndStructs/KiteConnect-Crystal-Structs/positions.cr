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

class Positions
  include JSON::Serializable

  property data : Data?

  property status : String?
end

class Data
  include JSON::Serializable

  property day : Array(Day)?

  property net : Array(Day)?
end

class Day
  include JSON::Serializable

  property average_price : Float64?

  @[JSON::Field(key: "buy_m2m")]
  property buy_m2_m : Int32?

  property buy_price : Float64?

  property buy_quantity : Int32?

  property buy_value : Int32?

  property close_price : Int32?

  property day_buy_price : Float64?

  property day_buy_quantity : Int32?

  property day_buy_value : Int32?

  property day_sell_price : Int32?

  property day_sell_quantity : Int32?

  property day_sell_value : Int32?

  property exchange : String?

  property instrument_token : Int32?

  property last_price : Float64?

  @[JSON::Field(key: "m2m")]
  property m2_m : Int32?

  property multiplier : Int32?

  property overnight_quantity : Int32?

  property pnl : Int32?

  property product : String?

  property quantity : Int32?

  property realised : Int32?

  @[JSON::Field(key: "sell_m2m")]
  property sell_m2_m : Int32?

  property sell_price : Int32?

  property sell_quantity : Int32?

  property sell_value : Int32?

  property tradingsymbol : String?

  property unrealised : Int32?

  property value : Int32?
end
