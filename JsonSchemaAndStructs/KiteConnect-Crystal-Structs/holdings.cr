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

class Holdings
  include JSON::Serializable

  property data : Array(Datum)?

  property status : String?
end

class Datum
  include JSON::Serializable

  property authorised_date : String?

  property authorised_quantity : Int32?

  property average_price : Float64?

  property close_price : Float64?

  property collateral_quantity : Int32?

  property collateral_type : String?

  property day_change : Float64?

  property day_change_percentage : Float64?

  property discrepancy : Bool?

  property exchange : String?

  property instrument_token : Int32?

  property isin : String?

  property last_price : Float64?

  property opening_quantity : Int32?

  property pnl : Float64?

  property price : Int32?

  property product : String?

  property quantity : Int32?

  property realised_quantity : Int32?

  property t1_quantity : Int32?

  property tradingsymbol : String?

  property used_quantity : Int32?
end
