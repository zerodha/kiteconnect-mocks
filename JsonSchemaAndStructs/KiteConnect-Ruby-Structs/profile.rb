# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   profile = Profile.from_json! "{…}"
#   puts profile.definitions.profile.required.first
#
# If from_json! succeeds, the value returned matches the schema.

require 'json'
require 'dry-types'
require 'dry-struct'

module Types
  include Dry::Types.module

  Bool   = Strict::Bool
  Hash   = Strict::Hash
  String = Strict::String
  Type   = Strict::String.enum("null", "string")
end

module Type
  Null   = "null"
  String = "string"
end

class AvatarURL < Dry::Struct
  attribute :avatar_url_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      avatar_url_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @avatar_url_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Meta < Dry::Struct
  attribute :ref, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      ref: d.fetch("$ref"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "$ref" => @ref,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Exchanges < Dry::Struct
  attribute :items,          AvatarURL
  attribute :exchanges_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:          AvatarURL.from_dynamic!(d.fetch("items")),
      exchanges_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @exchanges_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DataProperties < Dry::Struct
  attribute :avatar_url,           AvatarURL
  attribute :broker,               AvatarURL
  attribute :email,                AvatarURL
  attribute :exchanges,            Exchanges
  attribute :data_properties_meta, Meta
  attribute :order_types,          Exchanges
  attribute :products,             Exchanges
  attribute :user_id,              AvatarURL
  attribute :user_name,            AvatarURL
  attribute :user_shortname,       AvatarURL
  attribute :user_type,            AvatarURL

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      avatar_url:           AvatarURL.from_dynamic!(d.fetch("avatar_url")),
      broker:               AvatarURL.from_dynamic!(d.fetch("broker")),
      email:                AvatarURL.from_dynamic!(d.fetch("email")),
      exchanges:            Exchanges.from_dynamic!(d.fetch("exchanges")),
      data_properties_meta: Meta.from_dynamic!(d.fetch("meta")),
      order_types:          Exchanges.from_dynamic!(d.fetch("order_types")),
      products:             Exchanges.from_dynamic!(d.fetch("products")),
      user_id:              AvatarURL.from_dynamic!(d.fetch("user_id")),
      user_name:            AvatarURL.from_dynamic!(d.fetch("user_name")),
      user_shortname:       AvatarURL.from_dynamic!(d.fetch("user_shortname")),
      user_type:            AvatarURL.from_dynamic!(d.fetch("user_type")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "avatar_url"     => @avatar_url.to_dynamic,
      "broker"         => @broker.to_dynamic,
      "email"          => @email.to_dynamic,
      "exchanges"      => @exchanges.to_dynamic,
      "meta"           => @data_properties_meta.to_dynamic,
      "order_types"    => @order_types.to_dynamic,
      "products"       => @products.to_dynamic,
      "user_id"        => @user_id.to_dynamic,
      "user_name"      => @user_name.to_dynamic,
      "user_shortname" => @user_shortname.to_dynamic,
      "user_type"      => @user_type.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DataClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            DataProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :data_type,             Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            DataProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      data_type:             d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "additionalProperties" => @additional_properties,
      "properties"           => @properties.to_dynamic,
      "required"             => @required,
      "title"                => @title,
      "type"                 => @data_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MetaProperties < Dry::Struct
  attribute :demat_consent, AvatarURL

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      demat_consent: AvatarURL.from_dynamic!(d.fetch("demat_consent")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "demat_consent" => @demat_consent.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MetaClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            MetaProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :meta_class_type,       Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            MetaProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      meta_class_type:       d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "additionalProperties" => @additional_properties,
      "properties"           => @properties.to_dynamic,
      "required"             => @required,
      "title"                => @title,
      "type"                 => @meta_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class ProfileProperties < Dry::Struct
  attribute :data,   Meta
  attribute :status, AvatarURL

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   Meta.from_dynamic!(d.fetch("data")),
      status: AvatarURL.from_dynamic!(d.fetch("status")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "data"   => @data.to_dynamic,
      "status" => @status.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class ProfileClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            ProfileProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :profile_class_type,    Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            ProfileProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      profile_class_type:    d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "additionalProperties" => @additional_properties,
      "properties"           => @properties.to_dynamic,
      "required"             => @required,
      "title"                => @title,
      "type"                 => @profile_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :data,             DataClass
  attribute :definitions_meta, MetaClass
  attribute :profile,          ProfileClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:             DataClass.from_dynamic!(d.fetch("Data")),
      definitions_meta: MetaClass.from_dynamic!(d.fetch("Meta")),
      profile:          ProfileClass.from_dynamic!(d.fetch("Profile")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Data"    => @data.to_dynamic,
      "Meta"    => @definitions_meta.to_dynamic,
      "Profile" => @profile.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Profile < Dry::Struct
  attribute :ref,         Types::String
  attribute :schema,      Types::String
  attribute :definitions, Definitions

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      ref:         d.fetch("$ref"),
      schema:      d.fetch("$schema"),
      definitions: Definitions.from_dynamic!(d.fetch("definitions")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "$ref"        => @ref,
      "$schema"     => @schema,
      "definitions" => @definitions.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end
