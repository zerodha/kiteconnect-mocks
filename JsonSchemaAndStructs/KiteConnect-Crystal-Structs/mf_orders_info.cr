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

class MfOrdersInfo
  include JSON::Serializable

  property data : Data?

  property status : String?
end

class Data
  include JSON::Serializable

  property amount : Int32?

  property average_price : Int32?

  property exchange_order_id : Nil

  property exchange_timestamp : Nil

  property folio : Nil

  property fund : String?

  property last_price : Float64?

  property last_price_date : String?

  property order_id : String?

  property order_timestamp : String?

  property placed_by : String?

  property purchase_type : String?

  property quantity : Int32?

  property settlement_id : Nil

  property status : String?

  property status_message : String?

  property tag : Nil

  property tradingsymbol : String?

  property transaction_type : String?

  property variety : String?
end
