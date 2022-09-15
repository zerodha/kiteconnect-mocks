# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   margins_equity = MarginsEquity.from_json! "{…}"
#   puts margins_equity.definitions.margins_equity.required.first
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

class AdhocMargin < Dry::Struct
  attribute :adhoc_margin_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      adhoc_margin_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @adhoc_margin_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class AvailableProperties < Dry::Struct
  attribute :adhoc_margin,    AdhocMargin
  attribute :cash,            AdhocMargin
  attribute :collateral,      AdhocMargin
  attribute :intraday_payin,  AdhocMargin
  attribute :live_balance,    AdhocMargin
  attribute :opening_balance, AdhocMargin

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      adhoc_margin:    AdhocMargin.from_dynamic!(d.fetch("adhoc_margin")),
      cash:            AdhocMargin.from_dynamic!(d.fetch("cash")),
      collateral:      AdhocMargin.from_dynamic!(d.fetch("collateral")),
      intraday_payin:  AdhocMargin.from_dynamic!(d.fetch("intraday_payin")),
      live_balance:    AdhocMargin.from_dynamic!(d.fetch("live_balance")),
      opening_balance: AdhocMargin.from_dynamic!(d.fetch("opening_balance")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "adhoc_margin"    => @adhoc_margin.to_dynamic,
      "cash"            => @cash.to_dynamic,
      "collateral"      => @collateral.to_dynamic,
      "intraday_payin"  => @intraday_payin.to_dynamic,
      "live_balance"    => @live_balance.to_dynamic,
      "opening_balance" => @opening_balance.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Available < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            AvailableProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :available_type,        Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            AvailableProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      available_type:        d.fetch("type"),
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
      "type"                 => @available_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class AvailableClass < Dry::Struct
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

class Utilised < Dry::Struct
  attribute :additional_properties, AdhocMargin
  attribute :utilised_type,         Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: AdhocMargin.from_dynamic!(d.fetch("additionalProperties")),
      utilised_type:         d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "additionalProperties" => @additional_properties.to_dynamic,
      "type"                 => @utilised_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DataProperties < Dry::Struct
  attribute :available, AvailableClass
  attribute :enabled,   AdhocMargin
  attribute :net,       AdhocMargin
  attribute :utilised,  Utilised

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      available: AvailableClass.from_dynamic!(d.fetch("available")),
      enabled:   AdhocMargin.from_dynamic!(d.fetch("enabled")),
      net:       AdhocMargin.from_dynamic!(d.fetch("net")),
      utilised:  Utilised.from_dynamic!(d.fetch("utilised")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "available" => @available.to_dynamic,
      "enabled"   => @enabled.to_dynamic,
      "net"       => @net.to_dynamic,
      "utilised"  => @utilised.to_dynamic,
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

class MarginsEquityProperties < Dry::Struct
  attribute :data,   AvailableClass
  attribute :status, AdhocMargin

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   AvailableClass.from_dynamic!(d.fetch("data")),
      status: AdhocMargin.from_dynamic!(d.fetch("status")),
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

class MarginsEquityClass < Dry::Struct
  attribute :additional_properties,     Types::Bool
  attribute :properties,                MarginsEquityProperties
  attribute :required,                  Types.Array(Types::String)
  attribute :title,                     Types::String
  attribute :margins_equity_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:     d.fetch("additionalProperties"),
      properties:                MarginsEquityProperties.from_dynamic!(d.fetch("properties")),
      required:                  d.fetch("required"),
      title:                     d.fetch("title"),
      margins_equity_class_type: d.fetch("type"),
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
      "type"                 => @margins_equity_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :available,      Available
  attribute :data,           DataClass
  attribute :margins_equity, MarginsEquityClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      available:      Available.from_dynamic!(d.fetch("Available")),
      data:           DataClass.from_dynamic!(d.fetch("Data")),
      margins_equity: MarginsEquityClass.from_dynamic!(d.fetch("MarginsEquity")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Available"     => @available.to_dynamic,
      "Data"          => @data.to_dynamic,
      "MarginsEquity" => @margins_equity.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MarginsEquity < Dry::Struct
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
