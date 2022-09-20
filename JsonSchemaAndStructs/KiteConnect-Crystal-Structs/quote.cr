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

class Quote
  include JSON::Serializable

  property data : Hash(String, Datum)?

  property status : String?
end

class Datum
  include JSON::Serializable

  property average_price : Float64?

  property buy_quantity : Int32?

  property depth : Depth?

  property instrument_token : Int32?

  property last_price : Float64?

  property last_quantity : Int32?

  property last_trade_time : String?

  property lower_circuit_limit : Float64?

  property net_change : Int32?

  property ohlc : Ohlc?

  property oi : Int32?

  property oi_day_high : Int32?

  property oi_day_low : Int32?

  property sell_quantity : Int32?

  property timestamp : String?

  property upper_circuit_limit : Float64?

  property volume : Int32?
end

class Depth
  include JSON::Serializable

  property buy : Array(Buy)?

  property sell : Array(Buy)?
end

class Buy
  include JSON::Serializable

  property orders : Int32?

  property price : Float64?

  property quantity : Int32?
end

class Ohlc
  include JSON::Serializable

  property close : Float64?

  property high : Float64?

  property low : Float64?

  property open : Int32?
end
