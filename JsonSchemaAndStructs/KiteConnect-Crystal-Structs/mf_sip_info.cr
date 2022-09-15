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

  @[JSON::Field(key: "MFSIPInfo")]
  property mfsip_info : MfsipInfoClass

  @[JSON::Field(key: "StepUp")]
  property step_up : StepUpClass
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

  property completed_instalments : CompletedInstalments

  property created : Created

  property dividend_type : CompletedInstalments

  property frequency : CompletedInstalments

  property fund : CompletedInstalments

  property fund_source : CompletedInstalments

  property instalment_amount : CompletedInstalments

  property instalment_day : CompletedInstalments

  property instalments : CompletedInstalments

  property last_instalment : Created

  property next_instalment : Created

  property pending_instalments : CompletedInstalments

  property sip_id : CompletedInstalments

  property sip_reg_num : CompletedInstalments

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

  property format : String

  @[JSON::Field(key: "type")]
  property created_type : String
end

class StepUp
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class MfsipInfoClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MfsipInfoProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property mfsip_info_class_type : String
end

class MfsipInfoProperties
  include JSON::Serializable

  property data : StepUp

  property status : CompletedInstalments
end

class StepUpClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : StepUpProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property step_up_class_type : String
end

class StepUpProperties
  include JSON::Serializable

  @[JSON::Field(key: "15-02")]
  property the_1502 : CompletedInstalments
end
