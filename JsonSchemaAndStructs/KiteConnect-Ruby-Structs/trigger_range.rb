# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   trigger_range = TriggerRange.from_json! "{…}"
#   puts trigger_range.definitions.trigger_range.required.first
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

class NseInfy < Dry::Struct
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

class DataProperties < Dry::Struct
  attribute :nse_infy,     NseInfy
  attribute :nse_reliance, NseInfy

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      nse_infy:     NseInfy.from_dynamic!(d.fetch("NSE:INFY")),
      nse_reliance: NseInfy.from_dynamic!(d.fetch("NSE:RELIANCE")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "NSE:INFY"     => @nse_infy.to_dynamic,
      "NSE:RELIANCE" => @nse_reliance.to_dynamic,
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

class InstrumentToken < Dry::Struct
  attribute :instrument_token_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      instrument_token_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @instrument_token_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class NseProperties < Dry::Struct
  attribute :instrument_token, InstrumentToken
  attribute :lower,            InstrumentToken
  attribute :upper,            InstrumentToken

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      instrument_token: InstrumentToken.from_dynamic!(d.fetch("instrument_token")),
      lower:            InstrumentToken.from_dynamic!(d.fetch("lower")),
      upper:            InstrumentToken.from_dynamic!(d.fetch("upper")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "instrument_token" => @instrument_token.to_dynamic,
      "lower"            => @lower.to_dynamic,
      "upper"            => @upper.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Nse < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            NseProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :nse_type,              Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            NseProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      nse_type:              d.fetch("type"),
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
      "type"                 => @nse_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class TriggerRangeProperties < Dry::Struct
  attribute :data,   NseInfy
  attribute :status, InstrumentToken

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   NseInfy.from_dynamic!(d.fetch("data")),
      status: InstrumentToken.from_dynamic!(d.fetch("status")),
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

class TriggerRangeClass < Dry::Struct
  attribute :additional_properties,    Types::Bool
  attribute :properties,               TriggerRangeProperties
  attribute :required,                 Types.Array(Types::String)
  attribute :title,                    Types::String
  attribute :trigger_range_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:    d.fetch("additionalProperties"),
      properties:               TriggerRangeProperties.from_dynamic!(d.fetch("properties")),
      required:                 d.fetch("required"),
      title:                    d.fetch("title"),
      trigger_range_class_type: d.fetch("type"),
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
      "type"                 => @trigger_range_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :data,          DataClass
  attribute :nse,           Nse
  attribute :trigger_range, TriggerRangeClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:          DataClass.from_dynamic!(d.fetch("Data")),
      nse:           Nse.from_dynamic!(d.fetch("Nse")),
      trigger_range: TriggerRangeClass.from_dynamic!(d.fetch("TriggerRange")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Data"         => @data.to_dynamic,
      "Nse"          => @nse.to_dynamic,
      "TriggerRange" => @trigger_range.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class TriggerRange < Dry::Struct
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
