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

class MfSipInfo
  include JSON::Serializable

  property data : Data?

  property status : String?
end

class Data
  include JSON::Serializable

  property completed_instalments : Int32?

  property created : String?

  property dividend_type : String?

  property frequency : String?

  property fund : String?

  property fund_source : String?

  property instalment_amount : Int32?

  property instalment_day : Int32?

  property instalments : Int32?

  property last_instalment : String?

  property next_instalment : String?

  property pending_instalments : Int32?

  property sip_id : String?

  property sip_reg_num : Nil

  property sip_type : String?

  property status : String?

  property step_up : StepUp?

  property tag : String?

  property tradingsymbol : String?

  property transaction_type : String?

  property trigger_price : Int32?
end

class StepUp
  include JSON::Serializable

  @[JSON::Field(key: "15-02")]
  property the_1502 : Int32?
end
