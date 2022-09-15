# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   order_margins = OrderMargins.from_json! "{…}"
#   puts order_margins.definitions.pnl.required.first
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

class Additional < Dry::Struct
  attribute :additional_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @additional_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Pnl < Dry::Struct
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

class DatumProperties < Dry::Struct
  attribute :additional,            Additional
  attribute :bo,                    Additional
  attribute :cash,                  Additional
  attribute :exchange,              Additional
  attribute :exposure,              Additional
  attribute :option_premium,        Additional
  attribute :pnl,                   Pnl
  attribute :span,                  Additional
  attribute :total,                 Additional
  attribute :tradingsymbol,         Additional
  attribute :datum_properties_type, Additional
  attribute :var,                   Additional

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional:            Additional.from_dynamic!(d.fetch("additional")),
      bo:                    Additional.from_dynamic!(d.fetch("bo")),
      cash:                  Additional.from_dynamic!(d.fetch("cash")),
      exchange:              Additional.from_dynamic!(d.fetch("exchange")),
      exposure:              Additional.from_dynamic!(d.fetch("exposure")),
      option_premium:        Additional.from_dynamic!(d.fetch("option_premium")),
      pnl:                   Pnl.from_dynamic!(d.fetch("pnl")),
      span:                  Additional.from_dynamic!(d.fetch("span")),
      total:                 Additional.from_dynamic!(d.fetch("total")),
      tradingsymbol:         Additional.from_dynamic!(d.fetch("tradingsymbol")),
      datum_properties_type: Additional.from_dynamic!(d.fetch("type")),
      var:                   Additional.from_dynamic!(d.fetch("var")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "additional"     => @additional.to_dynamic,
      "bo"             => @bo.to_dynamic,
      "cash"           => @cash.to_dynamic,
      "exchange"       => @exchange.to_dynamic,
      "exposure"       => @exposure.to_dynamic,
      "option_premium" => @option_premium.to_dynamic,
      "pnl"            => @pnl.to_dynamic,
      "span"           => @span.to_dynamic,
      "total"          => @total.to_dynamic,
      "tradingsymbol"  => @tradingsymbol.to_dynamic,
      "type"           => @datum_properties_type.to_dynamic,
      "var"            => @var.to_dynamic,
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

class DataClass < Dry::Struct
  attribute :items,     Pnl
  attribute :data_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:     Pnl.from_dynamic!(d.fetch("items")),
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

class OrderMarginsProperties < Dry::Struct
  attribute :data,   DataClass
  attribute :status, Additional

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   DataClass.from_dynamic!(d.fetch("data")),
      status: Additional.from_dynamic!(d.fetch("status")),
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

class OrderMarginsClass < Dry::Struct
  attribute :additional_properties,    Types::Bool
  attribute :properties,               OrderMarginsProperties
  attribute :required,                 Types.Array(Types::String)
  attribute :title,                    Types::String
  attribute :order_margins_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:    d.fetch("additionalProperties"),
      properties:               OrderMarginsProperties.from_dynamic!(d.fetch("properties")),
      required:                 d.fetch("required"),
      title:                    d.fetch("title"),
      order_margins_class_type: d.fetch("type"),
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
      "type"                 => @order_margins_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class PnlProperties < Dry::Struct
  attribute :realised,   Additional
  attribute :unrealised, Additional

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      realised:   Additional.from_dynamic!(d.fetch("realised")),
      unrealised: Additional.from_dynamic!(d.fetch("unrealised")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "realised"   => @realised.to_dynamic,
      "unrealised" => @unrealised.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class PnlClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            PnlProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :pnl_class_type,        Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            PnlProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      pnl_class_type:        d.fetch("type"),
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
      "type"                 => @pnl_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :datum,         Datum
  attribute :order_margins, OrderMarginsClass
  attribute :pnl,           PnlClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      datum:         Datum.from_dynamic!(d.fetch("Datum")),
      order_margins: OrderMarginsClass.from_dynamic!(d.fetch("OrderMargins")),
      pnl:           PnlClass.from_dynamic!(d.fetch("Pnl")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Datum"        => @datum.to_dynamic,
      "OrderMargins" => @order_margins.to_dynamic,
      "Pnl"          => @pnl.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OrderMargins < Dry::Struct
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
