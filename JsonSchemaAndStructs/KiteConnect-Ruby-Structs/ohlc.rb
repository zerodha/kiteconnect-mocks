# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   ohlc = Ohlc.from_json! "{…}"
#   puts ohlc.definitions.ohlc_class.required.first
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
  attribute :nse_infy, NseInfy

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      nse_infy: NseInfy.from_dynamic!(d.fetch("NSE:INFY")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "NSE:INFY" => @nse_infy.to_dynamic,
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

class NseInfyProperties < Dry::Struct
  attribute :instrument_token, InstrumentToken
  attribute :last_price,       InstrumentToken
  attribute :ohlc,             NseInfy

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      instrument_token: InstrumentToken.from_dynamic!(d.fetch("instrument_token")),
      last_price:       InstrumentToken.from_dynamic!(d.fetch("last_price")),
      ohlc:             NseInfy.from_dynamic!(d.fetch("ohlc")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "instrument_token" => @instrument_token.to_dynamic,
      "last_price"       => @last_price.to_dynamic,
      "ohlc"             => @ohlc.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class NseInfyClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            NseInfyProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :nse_infy_class_type,   Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            NseInfyProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      nse_infy_class_type:   d.fetch("type"),
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
      "type"                 => @nse_infy_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OhlcProperties < Dry::Struct
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

class OhlcClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            OhlcProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :ohlc_class_type,       Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            OhlcProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      ohlc_class_type:       d.fetch("type"),
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
      "type"                 => @ohlc_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OhlcClassProperties < Dry::Struct
  attribute :close,                      InstrumentToken
  attribute :high,                       InstrumentToken
  attribute :low,                        InstrumentToken
  attribute :ohlc_class_properties_open, InstrumentToken

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      close:                      InstrumentToken.from_dynamic!(d.fetch("close")),
      high:                       InstrumentToken.from_dynamic!(d.fetch("high")),
      low:                        InstrumentToken.from_dynamic!(d.fetch("low")),
      ohlc_class_properties_open: InstrumentToken.from_dynamic!(d.fetch("open")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "close" => @close.to_dynamic,
      "high"  => @high.to_dynamic,
      "low"   => @low.to_dynamic,
      "open"  => @ohlc_class_properties_open.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OhlcClassClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            OhlcClassProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :ohlc_class_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            OhlcClassProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      ohlc_class_class_type: d.fetch("type"),
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
      "type"                 => @ohlc_class_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :data,       DataClass
  attribute :nse_infy,   NseInfyClass
  attribute :ohlc,       OhlcClass
  attribute :ohlc_class, OhlcClassClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:       DataClass.from_dynamic!(d.fetch("Data")),
      nse_infy:   NseInfyClass.from_dynamic!(d.fetch("NseInfy")),
      ohlc:       OhlcClass.from_dynamic!(d.fetch("Ohlc")),
      ohlc_class: OhlcClassClass.from_dynamic!(d.fetch("OhlcClass")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Data"      => @data.to_dynamic,
      "NseInfy"   => @nse_infy.to_dynamic,
      "Ohlc"      => @ohlc.to_dynamic,
      "OhlcClass" => @ohlc_class.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Ohlc < Dry::Struct
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
