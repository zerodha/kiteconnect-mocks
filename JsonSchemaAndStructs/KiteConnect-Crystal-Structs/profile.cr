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

  property data : Data?

  property status : String?
end

class Data
  include JSON::Serializable

  property avatar_url : Nil

  property broker : String?

  property email : String?

  property exchanges : Array(String)?

  property meta : Meta?

  property order_types : Array(String)?

  property products : Array(String)?

  property user_id : String?

  property user_name : String?

  property user_shortname : String?

  property user_type : String?
end

class Meta
  include JSON::Serializable

  property demat_consent : String?
end
