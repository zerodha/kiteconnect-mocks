# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   order_trades = OrderTrades.from_json! "{…}"
#   puts order_trades.definitions.order_trades.required.first
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
  Type   = Strict::String.enum("integer", "string")
end

module Type
  Integer = "integer"
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

class ExchangeTimestamp < Dry::Struct
  attribute :exchange_timestamp_format, Types::String
  attribute :exchange_timestamp_type,   Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      exchange_timestamp_format: d.fetch("format"),
      exchange_timestamp_type:   d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "format" => @exchange_timestamp_format,
      "type"   => @exchange_timestamp_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DatumProperties < Dry::Struct
  attribute :average_price,      AveragePrice
  attribute :exchange,           AveragePrice
  attribute :exchange_order_id,  AveragePrice
  attribute :exchange_timestamp, ExchangeTimestamp
  attribute :fill_timestamp,     ExchangeTimestamp
  attribute :instrument_token,   AveragePrice
  attribute :order_id,           AveragePrice
  attribute :order_timestamp,    ExchangeTimestamp
  attribute :product,            AveragePrice
  attribute :quantity,           AveragePrice
  attribute :trade_id,           ExchangeTimestamp
  attribute :tradingsymbol,      AveragePrice
  attribute :transaction_type,   AveragePrice

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      average_price:      AveragePrice.from_dynamic!(d.fetch("average_price")),
      exchange:           AveragePrice.from_dynamic!(d.fetch("exchange")),
      exchange_order_id:  AveragePrice.from_dynamic!(d.fetch("exchange_order_id")),
      exchange_timestamp: ExchangeTimestamp.from_dynamic!(d.fetch("exchange_timestamp")),
      fill_timestamp:     ExchangeTimestamp.from_dynamic!(d.fetch("fill_timestamp")),
      instrument_token:   AveragePrice.from_dynamic!(d.fetch("instrument_token")),
      order_id:           AveragePrice.from_dynamic!(d.fetch("order_id")),
      order_timestamp:    ExchangeTimestamp.from_dynamic!(d.fetch("order_timestamp")),
      product:            AveragePrice.from_dynamic!(d.fetch("product")),
      quantity:           AveragePrice.from_dynamic!(d.fetch("quantity")),
      trade_id:           ExchangeTimestamp.from_dynamic!(d.fetch("trade_id")),
      tradingsymbol:      AveragePrice.from_dynamic!(d.fetch("tradingsymbol")),
      transaction_type:   AveragePrice.from_dynamic!(d.fetch("transaction_type")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "average_price"      => @average_price.to_dynamic,
      "exchange"           => @exchange.to_dynamic,
      "exchange_order_id"  => @exchange_order_id.to_dynamic,
      "exchange_timestamp" => @exchange_timestamp.to_dynamic,
      "fill_timestamp"     => @fill_timestamp.to_dynamic,
      "instrument_token"   => @instrument_token.to_dynamic,
      "order_id"           => @order_id.to_dynamic,
      "order_timestamp"    => @order_timestamp.to_dynamic,
      "product"            => @product.to_dynamic,
      "quantity"           => @quantity.to_dynamic,
      "trade_id"           => @trade_id.to_dynamic,
      "tradingsymbol"      => @tradingsymbol.to_dynamic,
      "transaction_type"   => @transaction_type.to_dynamic,
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

class OrderTradesProperties < Dry::Struct
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

class OrderTradesClass < Dry::Struct
  attribute :additional_properties,   Types::Bool
  attribute :properties,              OrderTradesProperties
  attribute :required,                Types.Array(Types::String)
  attribute :title,                   Types::String
  attribute :order_trades_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:   d.fetch("additionalProperties"),
      properties:              OrderTradesProperties.from_dynamic!(d.fetch("properties")),
      required:                d.fetch("required"),
      title:                   d.fetch("title"),
      order_trades_class_type: d.fetch("type"),
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
      "type"                 => @order_trades_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :datum,        Datum
  attribute :order_trades, OrderTradesClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      datum:        Datum.from_dynamic!(d.fetch("Datum")),
      order_trades: OrderTradesClass.from_dynamic!(d.fetch("OrderTrades")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Datum"       => @datum.to_dynamic,
      "OrderTrades" => @order_trades.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OrderTrades < Dry::Struct
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
