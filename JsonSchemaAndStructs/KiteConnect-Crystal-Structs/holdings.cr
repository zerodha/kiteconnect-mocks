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

class Holdings
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

  @[JSON::Field(key: "Holdings")]
  property holdings : HoldingsClass
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

  property authorised_date : AuthorisedDate

  property authorised_quantity : AuthorisedQuantity

  property average_price : AuthorisedQuantity

  property close_price : AuthorisedQuantity

  property collateral_quantity : AuthorisedQuantity

  property collateral_type : AuthorisedQuantity

  property day_change : AuthorisedQuantity

  property day_change_percentage : AuthorisedQuantity

  property discrepancy : AuthorisedQuantity

  property exchange : AuthorisedQuantity

  property instrument_token : AuthorisedQuantity

  property isin : AuthorisedQuantity

  property last_price : AuthorisedQuantity

  property opening_quantity : AuthorisedQuantity

  property pnl : AuthorisedQuantity

  property price : AuthorisedQuantity

  property product : AuthorisedQuantity

  property quantity : AuthorisedQuantity

  property realised_quantity : AuthorisedQuantity

  property t1_quantity : AuthorisedQuantity

  property tradingsymbol : AuthorisedQuantity

  property used_quantity : AuthorisedQuantity
end

class AuthorisedDate
  include JSON::Serializable

  property format : String

  @[JSON::Field(key: "type")]
  property authorised_date_type : String
end

class AuthorisedQuantity
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property authorised_quantity_type : String
end

class HoldingsClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : HoldingsProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property holdings_class_type : String
end

class HoldingsProperties
  include JSON::Serializable

  property data : Data

  property status : AuthorisedQuantity
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
