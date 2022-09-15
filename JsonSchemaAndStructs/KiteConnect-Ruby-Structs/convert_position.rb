# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   convert_position = ConvertPosition.from_json! "{…}"
#   puts convert_position.definitions.convert_position.required.first
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
end

class DataClass < Dry::Struct
  attribute :data_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @data_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Properties < Dry::Struct
  attribute :data,   DataClass
  attribute :status, DataClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   DataClass.from_dynamic!(d.fetch("data")),
      status: DataClass.from_dynamic!(d.fetch("status")),
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

class ConvertPositionClass < Dry::Struct
  attribute :additional_properties,       Types::Bool
  attribute :properties,                  Properties
  attribute :required,                    Types.Array(Types::String)
  attribute :title,                       Types::String
  attribute :convert_position_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:       d.fetch("additionalProperties"),
      properties:                  Properties.from_dynamic!(d.fetch("properties")),
      required:                    d.fetch("required"),
      title:                       d.fetch("title"),
      convert_position_class_type: d.fetch("type"),
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
      "type"                 => @convert_position_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :convert_position, ConvertPositionClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      convert_position: ConvertPositionClass.from_dynamic!(d.fetch("ConvertPosition")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "ConvertPosition" => @convert_position.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class ConvertPosition < Dry::Struct
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
