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

class OrderTrades
  include JSON::Serializable

  property data : Array(Datum)?

  property status : String?
end

class Datum
  include JSON::Serializable

  property average_price : Int32?

  property exchange : String?

  property exchange_order_id : String?

  property exchange_timestamp : String?

  property fill_timestamp : String?

  property instrument_token : Int32?

  property order_id : String?

  property order_timestamp : String?

  property product : String?

  property quantity : Int32?

  property trade_id : String?

  property tradingsymbol : String?

  property transaction_type : String?
end
