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

alias MfInstruments = Array(MfInstrument)

class MfInstrument
  include JSON::Serializable

  property amc : String?

  property dividend_type : String?

  property last_price : Float64?

  property last_price_date : String?

  property minimum_additional_purchase_amount : Int32?

  property minimum_purchase_amount : Int32?

  property minimum_redemption_quantity : Float64?

  property name : String?

  property plan : String?

  property purchase_allowed : Int32?

  property purchase_amount_multiplier : Int32?

  property redemption_allowed : Int32?

  property redemption_quantity_multiplier : Float64?

  property scheme_type : String?

  property settlement_type : String?

  property tradingsymbol : String?
end
