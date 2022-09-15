# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   holdings = Holdings.from_json! "{…}"
#   puts holdings.definitions.holdings.required.first
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
  Type   = Strict::String.enum("boolean", "integer", "number", "string")
end

module Type
  Boolean = "boolean"
  Integer = "integer"
  Number  = "number"
  String  = "string"
end

class AuthorisedDate < Dry::Struct
  attribute :authorised_date_format, Types::String
  attribute :authorised_date_type,   Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      authorised_date_format: d.fetch("format"),
      authorised_date_type:   d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "format" => @authorised_date_format,
      "type"   => @authorised_date_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class AuthorisedQuantity < Dry::Struct
  attribute :authorised_quantity_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      authorised_quantity_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @authorised_quantity_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DatumProperties < Dry::Struct
  attribute :authorised_date,       AuthorisedDate
  attribute :authorised_quantity,   AuthorisedQuantity
  attribute :average_price,         AuthorisedQuantity
  attribute :close_price,           AuthorisedQuantity
  attribute :collateral_quantity,   AuthorisedQuantity
  attribute :collateral_type,       AuthorisedQuantity
  attribute :day_change,            AuthorisedQuantity
  attribute :day_change_percentage, AuthorisedQuantity
  attribute :discrepancy,           AuthorisedQuantity
  attribute :exchange,              AuthorisedQuantity
  attribute :instrument_token,      AuthorisedQuantity
  attribute :isin,                  AuthorisedQuantity
  attribute :last_price,            AuthorisedQuantity
  attribute :opening_quantity,      AuthorisedQuantity
  attribute :pnl,                   AuthorisedQuantity
  attribute :price,                 AuthorisedQuantity
  attribute :product,               AuthorisedQuantity
  attribute :quantity,              AuthorisedQuantity
  attribute :realised_quantity,     AuthorisedQuantity
  attribute :t1_quantity,           AuthorisedQuantity
  attribute :tradingsymbol,         AuthorisedQuantity
  attribute :used_quantity,         AuthorisedQuantity

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      authorised_date:       AuthorisedDate.from_dynamic!(d.fetch("authorised_date")),
      authorised_quantity:   AuthorisedQuantity.from_dynamic!(d.fetch("authorised_quantity")),
      average_price:         AuthorisedQuantity.from_dynamic!(d.fetch("average_price")),
      close_price:           AuthorisedQuantity.from_dynamic!(d.fetch("close_price")),
      collateral_quantity:   AuthorisedQuantity.from_dynamic!(d.fetch("collateral_quantity")),
      collateral_type:       AuthorisedQuantity.from_dynamic!(d.fetch("collateral_type")),
      day_change:            AuthorisedQuantity.from_dynamic!(d.fetch("day_change")),
      day_change_percentage: AuthorisedQuantity.from_dynamic!(d.fetch("day_change_percentage")),
      discrepancy:           AuthorisedQuantity.from_dynamic!(d.fetch("discrepancy")),
      exchange:              AuthorisedQuantity.from_dynamic!(d.fetch("exchange")),
      instrument_token:      AuthorisedQuantity.from_dynamic!(d.fetch("instrument_token")),
      isin:                  AuthorisedQuantity.from_dynamic!(d.fetch("isin")),
      last_price:            AuthorisedQuantity.from_dynamic!(d.fetch("last_price")),
      opening_quantity:      AuthorisedQuantity.from_dynamic!(d.fetch("opening_quantity")),
      pnl:                   AuthorisedQuantity.from_dynamic!(d.fetch("pnl")),
      price:                 AuthorisedQuantity.from_dynamic!(d.fetch("price")),
      product:               AuthorisedQuantity.from_dynamic!(d.fetch("product")),
      quantity:              AuthorisedQuantity.from_dynamic!(d.fetch("quantity")),
      realised_quantity:     AuthorisedQuantity.from_dynamic!(d.fetch("realised_quantity")),
      t1_quantity:           AuthorisedQuantity.from_dynamic!(d.fetch("t1_quantity")),
      tradingsymbol:         AuthorisedQuantity.from_dynamic!(d.fetch("tradingsymbol")),
      used_quantity:         AuthorisedQuantity.from_dynamic!(d.fetch("used_quantity")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "authorised_date"       => @authorised_date.to_dynamic,
      "authorised_quantity"   => @authorised_quantity.to_dynamic,
      "average_price"         => @average_price.to_dynamic,
      "close_price"           => @close_price.to_dynamic,
      "collateral_quantity"   => @collateral_quantity.to_dynamic,
      "collateral_type"       => @collateral_type.to_dynamic,
      "day_change"            => @day_change.to_dynamic,
      "day_change_percentage" => @day_change_percentage.to_dynamic,
      "discrepancy"           => @discrepancy.to_dynamic,
      "exchange"              => @exchange.to_dynamic,
      "instrument_token"      => @instrument_token.to_dynamic,
      "isin"                  => @isin.to_dynamic,
      "last_price"            => @last_price.to_dynamic,
      "opening_quantity"      => @opening_quantity.to_dynamic,
      "pnl"                   => @pnl.to_dynamic,
      "price"                 => @price.to_dynamic,
      "product"               => @product.to_dynamic,
      "quantity"              => @quantity.to_dynamic,
      "realised_quantity"     => @realised_quantity.to_dynamic,
      "t1_quantity"           => @t1_quantity.to_dynamic,
      "tradingsymbol"         => @tradingsymbol.to_dynamic,
      "used_quantity"         => @used_quantity.to_dynamic,
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

class HoldingsProperties < Dry::Struct
  attribute :data,   DataClass
  attribute :status, AuthorisedQuantity

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   DataClass.from_dynamic!(d.fetch("data")),
      status: AuthorisedQuantity.from_dynamic!(d.fetch("status")),
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

class HoldingsClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            HoldingsProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :holdings_class_type,   Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            HoldingsProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      holdings_class_type:   d.fetch("type"),
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
      "type"                 => @holdings_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :datum,    Datum
  attribute :holdings, HoldingsClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      datum:    Datum.from_dynamic!(d.fetch("Datum")),
      holdings: HoldingsClass.from_dynamic!(d.fetch("Holdings")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Datum"    => @datum.to_dynamic,
      "Holdings" => @holdings.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Holdings < Dry::Struct
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
