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

class MfOrders
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String

  @[JSON::Field(key: "$schema")]
  property schema : String

  property definitions : Definitions
end

class Definitions
  include JSON::Serializable

  @[JSON::Field(key: "Datum")]
  property datum : Datum

  @[JSON::Field(key: "MFOrders")]
  property mf_orders : MfOrdersClass
end

class Datum
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : DatumProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property datum_type : String
end

class DatumProperties
  include JSON::Serializable

  property amount : Amount

  property average_price : Amount

  property exchange_order_id : ExchangeOrderId

  property exchange_timestamp : ExchangeOrderId

  property folio : Amount

  property fund : Amount

  property last_price : Amount

  property last_price_date : LastPriceDate

  property order_id : LastPriceDate

  property order_timestamp : LastPriceDate

  property placed_by : Amount

  property purchase_type : Amount

  property quantity : Amount

  property settlement_id : ExchangeOrderId

  property status : Amount

  property status_message : Amount

  property tag : Tag

  property tradingsymbol : Amount

  property transaction_type : Amount

  property variety : Amount
end

class Amount
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property amount_type : String
end

class ExchangeOrderId
  include JSON::Serializable

  @[JSON::Field(key: "anyOf")]
  property any_of : Array(LastPriceDate)
end

class LastPriceDate
  include JSON::Serializable

  property format : String?

  @[JSON::Field(key: "type")]
  property last_price_date_type : String
end

class Tag
  include JSON::Serializable

  @[JSON::Field(key: "anyOf")]
  property any_of : Array(Amount)
end

class MfOrdersClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MfOrdersProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property mf_orders_class_type : String
end

class MfOrdersProperties
  include JSON::Serializable

  property data : Data

  property status : Amount
end

class Data
  include JSON::Serializable

  property items : Items

  @[JSON::Field(key: "type")]
  property data_type : String
end

class Items
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end
