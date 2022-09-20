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

class Orders
  include JSON::Serializable

  property data : Array(Datum)?

  property status : String?
end

class Datum
  include JSON::Serializable

  property average_price : Int32?

  property cancelled_quantity : Int32?

  property disclosed_quantity : Int32?

  property exchange : String?

  property exchange_order_id : String?

  property exchange_timestamp : String?

  property exchange_update_timestamp : String?

  property filled_quantity : Int32?

  property guid : String?

  property instrument_token : Int32?

  property market_protection : Int32?

  property meta : Meta?

  property modified : Bool?

  property order_id : String?

  property order_timestamp : String?

  property order_type : String?

  property parent_order_id : Nil

  property pending_quantity : Int32?

  property placed_by : String?

  property price : Int32?

  property product : String?

  property quantity : Int32?

  property status : String?

  property status_message : String?

  property status_message_raw : String?

  property tag : String?

  property tags : Array(String)?

  property tradingsymbol : String?

  property transaction_type : String?

  property trigger_price : Int32?

  property validity : String?

  property validity_ttl : Int32?

  property variety : String?
end

class Meta
  include JSON::Serializable

  property iceberg : Iceberg?
end

class Iceberg
  include JSON::Serializable

  property leg : Int32?

  property leg_quantity : Int32?

  property legs : Int32?

  property remaining_quantity : Int32?

  property total_quantity : Int32?
end
