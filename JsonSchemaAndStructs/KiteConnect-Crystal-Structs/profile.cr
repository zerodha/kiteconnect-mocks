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

class Profile
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

  @[JSON::Field(key: "Meta")]
  property meta : MetaClass

  @[JSON::Field(key: "Profile")]
  property profile : ProfileClass
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

  property avatar_url : AvatarUrl

  property broker : AvatarUrl

  property email : AvatarUrl

  property exchanges : Exchanges

  property meta : Meta

  property order_types : Exchanges

  property products : Exchanges

  property user_id : AvatarUrl

  property user_name : AvatarUrl

  property user_shortname : AvatarUrl

  property user_type : AvatarUrl
end

class AvatarUrl
  include JSON::Serializable

  @[JSON::Field(key: "type")]
  property avatar_url_type : String
end

class Exchanges
  include JSON::Serializable

  property items : AvatarUrl

  @[JSON::Field(key: "type")]
  property exchanges_type : String
end

class Meta
  include JSON::Serializable

  @[JSON::Field(key: "$ref")]
  property ref : String
end

class MetaClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : MetaProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property meta_class_type : String
end

class MetaProperties
  include JSON::Serializable

  property demat_consent : AvatarUrl
end

class ProfileClass
  include JSON::Serializable

  @[JSON::Field(key: "additionalProperties")]
  property additional_properties : Bool

  property properties : ProfileProperties

  property required : Array(String)

  property title : String

  @[JSON::Field(key: "type")]
  property profile_class_type : String
end

class ProfileProperties
  include JSON::Serializable

  property data : Meta

  property status : AvatarUrl
end
