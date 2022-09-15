# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   positions = Positions.from_json! "{…}"
#   puts positions.definitions.positions.required.first
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
  Type   = Strict::String.enum("integer", "number", "string")
end

class ItemsClass < Dry::Struct
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

class Day < Dry::Struct
  attribute :items,    ItemsClass
  attribute :day_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:    ItemsClass.from_dynamic!(d.fetch("items")),
      day_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @day_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DataProperties < Dry::Struct
  attribute :day, Day
  attribute :net, Day

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      day: Day.from_dynamic!(d.fetch("day")),
      net: Day.from_dynamic!(d.fetch("net")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "day" => @day.to_dynamic,
      "net" => @net.to_dynamic,
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

module Type
  Integer = "integer"
  Number  = "number"
  String  = "string"
end

class Property < Dry::Struct
  attribute :property_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      property_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @property_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DayClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            Types::Hash.meta(of: Property)
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :day_class_type,        Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            Types::Hash[d.fetch("properties")].map { |k, v| [k, Property.from_dynamic!(v)] }.to_h,
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      day_class_type:        d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "additionalProperties" => @additional_properties,
      "properties"           => @properties.map { |k, v| [k, v.to_dynamic] }.to_h,
      "required"             => @required,
      "title"                => @title,
      "type"                 => @day_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class PositionsProperties < Dry::Struct
  attribute :data,   ItemsClass
  attribute :status, Property

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   ItemsClass.from_dynamic!(d.fetch("data")),
      status: Property.from_dynamic!(d.fetch("status")),
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

class PositionsClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            PositionsProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :positions_class_type,  Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            PositionsProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      positions_class_type:  d.fetch("type"),
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
      "type"                 => @positions_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :data,      DataClass
  attribute :day,       DayClass
  attribute :positions, PositionsClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:      DataClass.from_dynamic!(d.fetch("Data")),
      day:       DayClass.from_dynamic!(d.fetch("Day")),
      positions: PositionsClass.from_dynamic!(d.fetch("Positions")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Data"      => @data.to_dynamic,
      "Day"       => @day.to_dynamic,
      "Positions" => @positions.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Positions < Dry::Struct
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
