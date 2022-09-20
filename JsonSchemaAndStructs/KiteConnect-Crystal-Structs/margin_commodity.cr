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

class MarginCommodity
  include JSON::Serializable

  property data : Data?

  property status : String?
end

class Data
  include JSON::Serializable

  property available : Available?

  property enabled : Bool?

  property net : Float64?

  property utilised : Hash(String, Float64)?
end

class Available
  include JSON::Serializable

  property adhoc_margin : Int32?

  property cash : Float64?

  property collateral : Int32?

  property intraday_payin : Int32?

  property live_balance : Float64?

  property opening_balance : Float64?
end
