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

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Data")]
  property data : Data

  @[JSON::Field(key: "MFOrdersInfo")]
  property mf_orders_info : MfOrdersInfoClass
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

  property amount : Amount

  property average_price : Amount

  property exchange_order_id : Amount

  property exchange_timestamp : Amount

  property folio : Amount

  property fund : Amount

  property last_price : Amount

  property last_price_date : LastPriceDate

  property order_id : LastPriceDate

  property order_timestamp : LastPriceDate

  property placed_by : Amount

  property purchase_type : Amount

  property quantity : Amount

  property settlement_id : Amount

  property status : Amount

  property status_message : Amount

  property tag : Amount

  property tradingsymbol : Amount

  property transaction_type : Amount

  property variety : Amount
end

class Amount
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property amount_type : String
end

class LastPriceDate
  include JSON::Serializable

  property format : String

  @[JSON::Field(key: "type")]
  property last_price_date_type : String
end

class MfOrdersInfoClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MfOrdersInfoProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property mf_orders_info_class_type : String
end

class MfOrdersInfoProperties
  include JSON::Serializable

  property data : DataClass

  property status : Amount
end

class DataClass
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end
