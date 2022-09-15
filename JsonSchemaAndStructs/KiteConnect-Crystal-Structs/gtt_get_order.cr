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

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Condition")]
  property condition : Condition

  @[JSON::Field(key: "Data")]
  property data : Data

  @[JSON::Field(key: "GttGetOrder")]
  property gtt_get_order : GttGetOrderClass

  @[JSON::Field(key: "Order")]
  property order : Order

  @[JSON::Field(key: "OrderResult")]
  property order_result : OrderResult

  @[JSON::Field(key: "Result")]
  property result : ResultClass
end

class Condition
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : ConditionProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property condition_type : String
end

class ConditionProperties
  include JSON::Serializable

  property exchange : Exchange

  property instrument_token : Exchange

  property last_price : Exchange

  property tradingsymbol : Exchange

  property trigger_values : TriggerValues
end

class Exchange
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property exchange_type : String
end

class TriggerValues
  include JSON::Serializable

  property items : Exchange

  @[JSON::Field(key: "type")]
  property trigger_values_type : String
end

class Data
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : DataProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property data_type : String
end

class DataProperties
  include JSON::Serializable

  property condition : ConditionClass

  property created_at : CreatedAt

  property expires_at : CreatedAt

  property id : Exchange

  property meta : Exchange

  property orders : Orders

  property parent_trigger : Exchange

  property status : Exchange

  @[JSON::Field(key: "type")]
  property data_properties_type : Exchange

  property updated_at : CreatedAt

  property user_id : Exchange
end

class ConditionClass
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class CreatedAt
  include JSON::Serializable

  property format : String

  @[JSON::Field(key: "type")]
  property created_at_type : String
end

class Orders
  include JSON::Serializable

  property items : ConditionClass

  @[JSON::Field(key: "type")]
  property orders_type : String
end

class GttGetOrderClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : GttGetOrderProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property gtt_get_order_class_type : String
end

class GttGetOrderProperties
  include JSON::Serializable

  property data : ConditionClass

  property status : Exchange
end

class Order
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OrderProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property order_type : String
end

class OrderProperties
  include JSON::Serializable

  property exchange : Exchange

  property order_type : Exchange

  property price : Exchange

  property product : Exchange

  property quantity : Exchange

  property result : Result

  property tradingsymbol : Exchange

  property transaction_type : Exchange
end

class Result
  include JSON::Serializable

  @[JSON::Field(key: "anyOf")]
  property any_of : Array(AnyOf)
end

class AnyOf
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String?

  @[JSON::Field(key: "type")]
  property any_of_type : String?
end

class OrderResult
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OrderResultProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property order_result_type : String
end

class OrderResultProperties
  include JSON::Serializable

  property order_id : Exchange

  property rejection_reason : Exchange

  property status : Exchange
end

class ResultClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : ResultProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property result_class_type : String
end

class ResultProperties
  include JSON::Serializable

  property account_id : Exchange

  property exchange : Exchange

  property meta : Exchange

  property order_result : ConditionClass

  property order_type : Exchange

  property price : Exchange

  property product : Exchange

  property quantity : Exchange

  property timestamp : CreatedAt

  property tradingsymbol : Exchange

  property transaction_type : Exchange

  property triggered_at : Exchange

  property validity : Exchange
end
