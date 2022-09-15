# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   quote = Quote.from_json! "{…}"
#   puts quote.definitions.quote.required.first
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

class Orders < Dry::Struct
  attribute :orders_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      orders_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @orders_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class BuyProperties < Dry::Struct
  attribute :orders,   Orders
  attribute :price,    Orders
  attribute :quantity, Orders

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      orders:   Orders.from_dynamic!(d.fetch("orders")),
      price:    Orders.from_dynamic!(d.fetch("price")),
      quantity: Orders.from_dynamic!(d.fetch("quantity")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "orders"   => @orders.to_dynamic,
      "price"    => @price.to_dynamic,
      "quantity" => @quantity.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Buy < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            BuyProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :buy_type,              Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            BuyProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      buy_type:              d.fetch("type"),
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
      "type"                 => @buy_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
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

class BuyClass < Dry::Struct
  attribute :items,          NseInfy
  attribute :buy_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:          NseInfy.from_dynamic!(d.fetch("items")),
      buy_class_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @buy_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DepthProperties < Dry::Struct
  attribute :buy,  BuyClass
  attribute :sell, BuyClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      buy:  BuyClass.from_dynamic!(d.fetch("buy")),
      sell: BuyClass.from_dynamic!(d.fetch("sell")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "buy"  => @buy.to_dynamic,
      "sell" => @sell.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Depth < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            DepthProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :depth_type,            Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            DepthProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      depth_type:            d.fetch("type"),
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
      "type"                 => @depth_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class LastTradeTime < Dry::Struct
  attribute :last_trade_time_format, Types::String
  attribute :last_trade_time_type,   Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      last_trade_time_format: d.fetch("format"),
      last_trade_time_type:   d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "format" => @last_trade_time_format,
      "type"   => @last_trade_time_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class NseInfyProperties < Dry::Struct
  attribute :average_price,       Orders
  attribute :buy_quantity,        Orders
  attribute :depth,               NseInfy
  attribute :instrument_token,    Orders
  attribute :last_price,          Orders
  attribute :last_quantity,       Orders
  attribute :last_trade_time,     LastTradeTime
  attribute :lower_circuit_limit, Orders
  attribute :net_change,          Orders
  attribute :ohlc,                NseInfy
  attribute :oi,                  Orders
  attribute :oi_day_high,         Orders
  attribute :oi_day_low,          Orders
  attribute :sell_quantity,       Orders
  attribute :timestamp,           LastTradeTime
  attribute :upper_circuit_limit, Orders
  attribute :volume,              Orders

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      average_price:       Orders.from_dynamic!(d.fetch("average_price")),
      buy_quantity:        Orders.from_dynamic!(d.fetch("buy_quantity")),
      depth:               NseInfy.from_dynamic!(d.fetch("depth")),
      instrument_token:    Orders.from_dynamic!(d.fetch("instrument_token")),
      last_price:          Orders.from_dynamic!(d.fetch("last_price")),
      last_quantity:       Orders.from_dynamic!(d.fetch("last_quantity")),
      last_trade_time:     LastTradeTime.from_dynamic!(d.fetch("last_trade_time")),
      lower_circuit_limit: Orders.from_dynamic!(d.fetch("lower_circuit_limit")),
      net_change:          Orders.from_dynamic!(d.fetch("net_change")),
      ohlc:                NseInfy.from_dynamic!(d.fetch("ohlc")),
      oi:                  Orders.from_dynamic!(d.fetch("oi")),
      oi_day_high:         Orders.from_dynamic!(d.fetch("oi_day_high")),
      oi_day_low:          Orders.from_dynamic!(d.fetch("oi_day_low")),
      sell_quantity:       Orders.from_dynamic!(d.fetch("sell_quantity")),
      timestamp:           LastTradeTime.from_dynamic!(d.fetch("timestamp")),
      upper_circuit_limit: Orders.from_dynamic!(d.fetch("upper_circuit_limit")),
      volume:              Orders.from_dynamic!(d.fetch("volume")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "average_price"       => @average_price.to_dynamic,
      "buy_quantity"        => @buy_quantity.to_dynamic,
      "depth"               => @depth.to_dynamic,
      "instrument_token"    => @instrument_token.to_dynamic,
      "last_price"          => @last_price.to_dynamic,
      "last_quantity"       => @last_quantity.to_dynamic,
      "last_trade_time"     => @last_trade_time.to_dynamic,
      "lower_circuit_limit" => @lower_circuit_limit.to_dynamic,
      "net_change"          => @net_change.to_dynamic,
      "ohlc"                => @ohlc.to_dynamic,
      "oi"                  => @oi.to_dynamic,
      "oi_day_high"         => @oi_day_high.to_dynamic,
      "oi_day_low"          => @oi_day_low.to_dynamic,
      "sell_quantity"       => @sell_quantity.to_dynamic,
      "timestamp"           => @timestamp.to_dynamic,
      "upper_circuit_limit" => @upper_circuit_limit.to_dynamic,
      "volume"              => @volume.to_dynamic,
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
  attribute :close,                Orders
  attribute :high,                 Orders
  attribute :low,                  Orders
  attribute :ohlc_properties_open, Orders

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      close:                Orders.from_dynamic!(d.fetch("close")),
      high:                 Orders.from_dynamic!(d.fetch("high")),
      low:                  Orders.from_dynamic!(d.fetch("low")),
      ohlc_properties_open: Orders.from_dynamic!(d.fetch("open")),
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
      "open"  => @ohlc_properties_open.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Ohlc < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            OhlcProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :ohlc_type,             Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            OhlcProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      ohlc_type:             d.fetch("type"),
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
      "type"                 => @ohlc_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class QuoteProperties < Dry::Struct
  attribute :data,   NseInfy
  attribute :status, Orders

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   NseInfy.from_dynamic!(d.fetch("data")),
      status: Orders.from_dynamic!(d.fetch("status")),
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

class QuoteClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            QuoteProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :quote_class_type,      Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            QuoteProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      quote_class_type:      d.fetch("type"),
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
      "type"                 => @quote_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :buy,      Buy
  attribute :data,     DataClass
  attribute :depth,    Depth
  attribute :nse_infy, NseInfyClass
  attribute :ohlc,     Ohlc
  attribute :quote,    QuoteClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      buy:      Buy.from_dynamic!(d.fetch("Buy")),
      data:     DataClass.from_dynamic!(d.fetch("Data")),
      depth:    Depth.from_dynamic!(d.fetch("Depth")),
      nse_infy: NseInfyClass.from_dynamic!(d.fetch("NseInfy")),
      ohlc:     Ohlc.from_dynamic!(d.fetch("Ohlc")),
      quote:    QuoteClass.from_dynamic!(d.fetch("Quote")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Buy"     => @buy.to_dynamic,
      "Data"    => @data.to_dynamic,
      "Depth"   => @depth.to_dynamic,
      "NseInfy" => @nse_infy.to_dynamic,
      "Ohlc"    => @ohlc.to_dynamic,
      "Quote"   => @quote.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Quote < Dry::Struct
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
