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

class Postback
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Meta")]
  property meta : Meta

  @[JSON::Field(key: "Postback")]
  property postback : PostbackClass
end

class Meta
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property title : String

  @[JSON::Field(key: "type")]
  property meta_type : String
end

class PostbackClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : Properties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property postback_class_type : String
end

class Properties
  include JSON::Serializable

  property app_id : AppId

  property average_price : AppId

  property cancelled_quantity : AppId

  property checksum : AppId

  property disclosed_quantity : AppId

  property exchange : AppId

  property exchange_order_id : AppId

  property exchange_timestamp : Timestamp

  property exchange_update_timestamp : Timestamp

  property filled_quantity : AppId

  property guid : AppId

  property instrument_token : AppId

  property market_protection : AppId

  property meta : MetaClass

  property order_id : AppId

  property order_timestamp : Timestamp

  property order_type : AppId

  property parent_order_id : AppId

  property pending_quantity : AppId

  property placed_by : AppId

  property price : AppId

  property product : AppId

  property quantity : AppId

  property status : AppId

  property status_message : AppId

  property status_message_raw : AppId

  property tag : AppId

  property tradingsymbol : AppId

  property transaction_type : AppId

  property trigger_price : AppId

  property unfilled_quantity : AppId

  property user_id : AppId

  property validity : AppId

  property variety : AppId
end

class AppId
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property app_id_type : String
end

class Timestamp
  include JSON::Serializable

  property format : String

  @[JSON::Field(key: "type")]
  property timestamp_type : String
end

class MetaClass
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end
