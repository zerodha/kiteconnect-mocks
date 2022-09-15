# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   basket_margins = BasketMargins.from_json! "{…}"
#   puts basket_margins.definitions.pnl.required.first
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

class DataClass < Dry::Struct
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

module Type
  Integer = "integer"
  Number  = "number"
  String  = "string"
end

class Status < Dry::Struct
  attribute :status_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      status_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @status_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class BasketMarginsProperties < Dry::Struct
  attribute :data,   DataClass
  attribute :status, Status

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   DataClass.from_dynamic!(d.fetch("data")),
      status: Status.from_dynamic!(d.fetch("status")),
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

class BasketMarginsClass < Dry::Struct
  attribute :additional_properties,     Types::Bool
  attribute :properties,                BasketMarginsProperties
  attribute :required,                  Types.Array(Types::String)
  attribute :title,                     Types::String
  attribute :basket_margins_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:     d.fetch("additionalProperties"),
      properties:                BasketMarginsProperties.from_dynamic!(d.fetch("properties")),
      required:                  d.fetch("required"),
      title:                     d.fetch("title"),
      basket_margins_class_type: d.fetch("type"),
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
      "type"                 => @basket_margins_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Orders < Dry::Struct
  attribute :items,       DataClass
  attribute :orders_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:       DataClass.from_dynamic!(d.fetch("items")),
      orders_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @orders_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DataProperties < Dry::Struct
  attribute :final,   DataClass
  attribute :initial, DataClass
  attribute :orders,  Orders

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      final:   DataClass.from_dynamic!(d.fetch("final")),
      initial: DataClass.from_dynamic!(d.fetch("initial")),
      orders:  Orders.from_dynamic!(d.fetch("orders")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "final"   => @final.to_dynamic,
      "initial" => @initial.to_dynamic,
      "orders"  => @orders.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DefinitionsData < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            DataProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :data_class_type,       Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            DataProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      data_class_type:       d.fetch("type"),
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
      "type"                 => @data_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class FinalProperties < Dry::Struct
  attribute :additional,            Status
  attribute :bo,                    Status
  attribute :cash,                  Status
  attribute :exchange,              Status
  attribute :exposure,              Status
  attribute :option_premium,        Status
  attribute :pnl,                   DataClass
  attribute :span,                  Status
  attribute :total,                 Status
  attribute :tradingsymbol,         Status
  attribute :final_properties_type, Status
  attribute :var,                   Status

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional:            Status.from_dynamic!(d.fetch("additional")),
      bo:                    Status.from_dynamic!(d.fetch("bo")),
      cash:                  Status.from_dynamic!(d.fetch("cash")),
      exchange:              Status.from_dynamic!(d.fetch("exchange")),
      exposure:              Status.from_dynamic!(d.fetch("exposure")),
      option_premium:        Status.from_dynamic!(d.fetch("option_premium")),
      pnl:                   DataClass.from_dynamic!(d.fetch("pnl")),
      span:                  Status.from_dynamic!(d.fetch("span")),
      total:                 Status.from_dynamic!(d.fetch("total")),
      tradingsymbol:         Status.from_dynamic!(d.fetch("tradingsymbol")),
      final_properties_type: Status.from_dynamic!(d.fetch("type")),
      var:                   Status.from_dynamic!(d.fetch("var")),
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
      "type"           => @final_properties_type.to_dynamic,
      "var"            => @var.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Final < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            FinalProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :final_type,            Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            FinalProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      final_type:            d.fetch("type"),
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
      "type"                 => @final_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class PnlProperties < Dry::Struct
  attribute :realised,   Status
  attribute :unrealised, Status

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      realised:   Status.from_dynamic!(d.fetch("realised")),
      unrealised: Status.from_dynamic!(d.fetch("unrealised")),
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

class Pnl < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            PnlProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :pnl_type,              Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            PnlProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      pnl_type:              d.fetch("type"),
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
      "type"                 => @pnl_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :basket_margins, BasketMarginsClass
  attribute :data,           DefinitionsData
  attribute :final,          Final
  attribute :pnl,            Pnl

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      basket_margins: BasketMarginsClass.from_dynamic!(d.fetch("BasketMargins")),
      data:           DefinitionsData.from_dynamic!(d.fetch("Data")),
      final:          Final.from_dynamic!(d.fetch("Final")),
      pnl:            Pnl.from_dynamic!(d.fetch("Pnl")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "BasketMargins" => @basket_margins.to_dynamic,
      "Data"          => @data.to_dynamic,
      "Final"         => @final.to_dynamic,
      "Pnl"           => @pnl.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class BasketMargins < Dry::Struct
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
