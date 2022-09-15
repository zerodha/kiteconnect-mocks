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

class MfHoldings
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

  @[JSON::Field(key: "MFHoldings")]
  property mf_holdings : MfHoldingsClass
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

  property folio : AveragePrice

  property fund : AveragePrice

  property last_price : AveragePrice

  property last_price_date : AveragePrice

  property pledged_quantity : AveragePrice

  property pnl : AveragePrice

  property quantity : AveragePrice

  property tradingsymbol : AveragePrice
end

class AveragePrice
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property average_price_type : String
end

class MfHoldingsClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MfHoldingsProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property mf_holdings_class_type : String
end

class MfHoldingsProperties
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
