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

alias TickerQuote = Array(TriggerRangeElement)

class TriggerRangeElement
  include JSON::Serializable

  property average_traded_price : Float64?

  property change : Float64?

  property instrument_token : Int32?

  property last_price : Int32?

  property last_traded_quantity : Int32?

  property mode : String?

  property ohlc : Ohlc?

  property total_buy_quantity : Int32?

  property total_sell_quantity : Int32?

  property tradable : Bool?

  property volume_traded : Int32?
end

class Ohlc
  include JSON::Serializable

  property close : Int32?

  property high : Int32?

  property low : Int32?

  property open : Int32?
end
