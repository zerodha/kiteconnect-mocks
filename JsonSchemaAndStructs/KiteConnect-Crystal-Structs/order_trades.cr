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

class OrderTrades
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

  @[JSON::Field(key: "OrderTrades")]
  property order_trades : OrderTradesClass
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

  property average_price : AveragePrice

  property exchange : AveragePrice

  property exchange_order_id : AveragePrice

  property exchange_timestamp : ExchangeTimestamp

  property fill_timestamp : ExchangeTimestamp

  property instrument_token : AveragePrice

  property order_id : AveragePrice

  property order_timestamp : ExchangeTimestamp

  property product : AveragePrice

  property quantity : AveragePrice

  property trade_id : ExchangeTimestamp

  property tradingsymbol : AveragePrice

  property transaction_type : AveragePrice
end

class AveragePrice
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property average_price_type : String
end

class ExchangeTimestamp
  include JSON::Serializable

  property format : String

  @[JSON::Field(key: "type")]
  property exchange_timestamp_type : String
end

class OrderTradesClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : OrderTradesProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property order_trades_class_type : String
end

class OrderTradesProperties
  include JSON::Serializable

  property data : Data

  property status : AveragePrice
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
