# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   mf_holdings = MFHoldings.from_json! "{…}"
#   puts mf_holdings.definitions.mf_holdings.required.first
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

module Type
  Integer = "integer"
  Number  = "number"
  String  = "string"
end

class AveragePrice < Dry::Struct
  attribute :average_price_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      average_price_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @average_price_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DatumProperties < Dry::Struct
  attribute :average_price,    AveragePrice
  attribute :folio,            AveragePrice
  attribute :fund,             AveragePrice
  attribute :last_price,       AveragePrice
  attribute :last_price_date,  AveragePrice
  attribute :pledged_quantity, AveragePrice
  attribute :pnl,              AveragePrice
  attribute :quantity,         AveragePrice
  attribute :tradingsymbol,    AveragePrice

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      average_price:    AveragePrice.from_dynamic!(d.fetch("average_price")),
      folio:            AveragePrice.from_dynamic!(d.fetch("folio")),
      fund:             AveragePrice.from_dynamic!(d.fetch("fund")),
      last_price:       AveragePrice.from_dynamic!(d.fetch("last_price")),
      last_price_date:  AveragePrice.from_dynamic!(d.fetch("last_price_date")),
      pledged_quantity: AveragePrice.from_dynamic!(d.fetch("pledged_quantity")),
      pnl:              AveragePrice.from_dynamic!(d.fetch("pnl")),
      quantity:         AveragePrice.from_dynamic!(d.fetch("quantity")),
      tradingsymbol:    AveragePrice.from_dynamic!(d.fetch("tradingsymbol")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "average_price"    => @average_price.to_dynamic,
      "folio"            => @folio.to_dynamic,
      "fund"             => @fund.to_dynamic,
      "last_price"       => @last_price.to_dynamic,
      "last_price_date"  => @last_price_date.to_dynamic,
      "pledged_quantity" => @pledged_quantity.to_dynamic,
      "pnl"              => @pnl.to_dynamic,
      "quantity"         => @quantity.to_dynamic,
      "tradingsymbol"    => @tradingsymbol.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Datum < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            DatumProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :datum_type,            Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            DatumProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      datum_type:            d.fetch("type"),
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
      "type"                 => @datum_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Items < Dry::Struct
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

class DataClass < Dry::Struct
  attribute :items,     Items
  attribute :data_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:     Items.from_dynamic!(d.fetch("items")),
      data_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @data_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MFHoldingsProperties < Dry::Struct
  attribute :data,   DataClass
  attribute :status, AveragePrice

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   DataClass.from_dynamic!(d.fetch("data")),
      status: AveragePrice.from_dynamic!(d.fetch("status")),
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

class MFHoldingsClass < Dry::Struct
  attribute :additional_properties,  Types::Bool
  attribute :properties,             MFHoldingsProperties
  attribute :required,               Types.Array(Types::String)
  attribute :title,                  Types::String
  attribute :mf_holdings_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:  d.fetch("additionalProperties"),
      properties:             MFHoldingsProperties.from_dynamic!(d.fetch("properties")),
      required:               d.fetch("required"),
      title:                  d.fetch("title"),
      mf_holdings_class_type: d.fetch("type"),
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
      "type"                 => @mf_holdings_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :datum,       Datum
  attribute :mf_holdings, MFHoldingsClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      datum:       Datum.from_dynamic!(d.fetch("Datum")),
      mf_holdings: MFHoldingsClass.from_dynamic!(d.fetch("MFHoldings")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Datum"      => @datum.to_dynamic,
      "MFHoldings" => @mf_holdings.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MFHoldings < Dry::Struct
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
