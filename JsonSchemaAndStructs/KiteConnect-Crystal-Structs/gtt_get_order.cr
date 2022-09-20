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

class GttGetOrder
  include JSON::Serializable

  property data : Data?

  property status : String?
end

class Data
  include JSON::Serializable

  property condition : Condition?

  property created_at : String?

  property expires_at : String?

  property id : Int32?

  property meta : Nil

  property orders : Array(Order)?

  property parent_trigger : Nil

  property status : String?

  @[JSON::Field(key: "type")]
  property data_type : String?

  property updated_at : String?

  property user_id : String?
end

class Condition
  include JSON::Serializable

  property exchange : String?

  property instrument_token : Int32?

  property last_price : Float64?

  property tradingsymbol : String?

  property trigger_values : Array(Float64)?
end

class Order
  include JSON::Serializable

  property exchange : String?

  property order_type : String?

  property price : Int32?

  property product : String?

  property quantity : Int32?

  property result : Result?

  property tradingsymbol : String?

  property transaction_type : String?
end

class Result
  include JSON::Serializable

  property account_id : String?

  property exchange : String?

  property meta : String?

  property order_result : OrderResult?

  property order_type : String?

  property price : Int32?

  property product : String?

  property quantity : Int32?

  property timestamp : String?

  property tradingsymbol : String?

  property transaction_type : String?

  property triggered_at : Float64?

  property validity : String?
end

class OrderResult
  include JSON::Serializable

  property order_id : String?

  property rejection_reason : String?

  property status : String?
end
