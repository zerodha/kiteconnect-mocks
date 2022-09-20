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

class MfSips
  include JSON::Serializable

  property data : Array(Datum)?
end

class Datum
  include JSON::Serializable

  property completed_instalments : Int32?

  property created : String?

  property dividend_type : String?

  property frequency : String?

  property fund : String?

  property instalment_amount : Int32?

  property instalment_day : Int32?

  property instalments : Int32?

  property last_instalment : String?

  property next_instalment : String?

  property pending_instalments : Int32?

  property sip_id : String?

  property sip_reg_num : String?

  property sip_type : String?

  property status : String?

  property step_up : Hash(String, Int32)?

  property tag : String?

  property tradingsymbol : String?

  property transaction_type : String?

  property trigger_price : Int32?
end
