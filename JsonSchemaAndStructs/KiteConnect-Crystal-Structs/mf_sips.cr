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

  @[JSON::Field(key: "MFSips")]
  property mf_sips : MfSipsClass
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

  property completed_instalments : CompletedInstalments

  property created : Created

  property dividend_type : CompletedInstalments

  property frequency : CompletedInstalments

  property fund : CompletedInstalments

  property instalment_amount : CompletedInstalments

  property instalment_day : CompletedInstalments

  property instalments : CompletedInstalments

  property last_instalment : Created

  property next_instalment : Created

  property pending_instalments : CompletedInstalments

  property sip_id : CompletedInstalments

  property sip_reg_num : SipRegNum

  property sip_type : CompletedInstalments

  property status : CompletedInstalments

  property step_up : StepUp

  property tag : CompletedInstalments

  property tradingsymbol : CompletedInstalments

  property transaction_type : CompletedInstalments

  property trigger_price : CompletedInstalments
end

class CompletedInstalments
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property completed_instalments_type : String
end

class Created
  include JSON::Serializable

  property format : String?

  @[JSON::Field(key: "type")]
  property created_type : String
end

class SipRegNum
  include JSON::Serializable

  @[JSON::Field(key: "anyOf")]
  property any_of : Array(Created)
end

class StepUp
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : CompletedInstalments

  @[JSON::Field(key: "type")]
  property step_up_type : String
end

class MfSipsClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MfSipsProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property mf_sips_class_type : String
end

class MfSipsProperties
  include JSON::Serializable

  property data : Data
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
