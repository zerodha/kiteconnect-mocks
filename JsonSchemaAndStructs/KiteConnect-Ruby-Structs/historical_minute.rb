# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   historical_minute = HistoricalMinute.from_json! "{…}"
#   puts historical_minute.definitions.historical_minute.required.first
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

class AnyOf < Dry::Struct
  attribute :any_of_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      any_of_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @any_of_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Candle < Dry::Struct
  attribute :any_of, Types.Array(AnyOf)
  attribute :title,  Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      any_of: d.fetch("anyOf").map { |x| AnyOf.from_dynamic!(x) },
      title:  d.fetch("title"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "anyOf" => @any_of.map { |x| x.to_dynamic },
      "title" => @title,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
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

class Items < Dry::Struct
  attribute :items,      ItemsClass
  attribute :items_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:      ItemsClass.from_dynamic!(d.fetch("items")),
      items_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @items_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Candles < Dry::Struct
  attribute :items,        Items
  attribute :candles_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:        Items.from_dynamic!(d.fetch("items")),
      candles_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @candles_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DataProperties < Dry::Struct
  attribute :candles, Candles

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      candles: Candles.from_dynamic!(d.fetch("candles")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "candles" => @candles.to_dynamic,
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

class HistoricalMinuteProperties < Dry::Struct
  attribute :data,   ItemsClass
  attribute :status, AnyOf

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   ItemsClass.from_dynamic!(d.fetch("data")),
      status: AnyOf.from_dynamic!(d.fetch("status")),
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

class HistoricalMinuteClass < Dry::Struct
  attribute :additional_properties,        Types::Bool
  attribute :properties,                   HistoricalMinuteProperties
  attribute :required,                     Types.Array(Types::String)
  attribute :title,                        Types::String
  attribute :historical_minute_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:        d.fetch("additionalProperties"),
      properties:                   HistoricalMinuteProperties.from_dynamic!(d.fetch("properties")),
      required:                     d.fetch("required"),
      title:                        d.fetch("title"),
      historical_minute_class_type: d.fetch("type"),
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
      "type"                 => @historical_minute_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :candle,            Candle
  attribute :data,              DataClass
  attribute :historical_minute, HistoricalMinuteClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      candle:            Candle.from_dynamic!(d.fetch("Candle")),
      data:              DataClass.from_dynamic!(d.fetch("Data")),
      historical_minute: HistoricalMinuteClass.from_dynamic!(d.fetch("HistoricalMinute")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Candle"           => @candle.to_dynamic,
      "Data"             => @data.to_dynamic,
      "HistoricalMinute" => @historical_minute.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class HistoricalMinute < Dry::Struct
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
