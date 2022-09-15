# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   mf_orders = MFOrders.from_json! "{…}"
#   puts mf_orders.definitions.mf_orders.required.first
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
  Type   = Strict::String.enum("null", "number", "string")
end

module Type
  Null   = "null"
  Number = "number"
  String = "string"
end

class Amount < Dry::Struct
  attribute :amount_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      amount_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @amount_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class LastPriceDate < Dry::Struct
  attribute :last_price_date_format, Types::String.optional
  attribute :last_price_date_type,   Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      last_price_date_format: d["format"],
      last_price_date_type:   d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "format" => @last_price_date_format,
      "type"   => @last_price_date_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class ExchangeOrderID < Dry::Struct
  attribute :any_of, Types.Array(LastPriceDate)

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      any_of: d.fetch("anyOf").map { |x| LastPriceDate.from_dynamic!(x) },
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "anyOf" => @any_of.map { |x| x.to_dynamic },
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Tag < Dry::Struct
  attribute :any_of, Types.Array(Amount)

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      any_of: d.fetch("anyOf").map { |x| Amount.from_dynamic!(x) },
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "anyOf" => @any_of.map { |x| x.to_dynamic },
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DatumProperties < Dry::Struct
  attribute :amount,             Amount
  attribute :average_price,      Amount
  attribute :exchange_order_id,  ExchangeOrderID
  attribute :exchange_timestamp, ExchangeOrderID
  attribute :folio,              Amount
  attribute :fund,               Amount
  attribute :last_price,         Amount
  attribute :last_price_date,    LastPriceDate
  attribute :order_id,           LastPriceDate
  attribute :order_timestamp,    LastPriceDate
  attribute :placed_by,          Amount
  attribute :purchase_type,      Amount
  attribute :quantity,           Amount
  attribute :settlement_id,      ExchangeOrderID
  attribute :status,             Amount
  attribute :status_message,     Amount
  attribute :tag,                Tag
  attribute :tradingsymbol,      Amount
  attribute :transaction_type,   Amount
  attribute :variety,            Amount

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      amount:             Amount.from_dynamic!(d.fetch("amount")),
      average_price:      Amount.from_dynamic!(d.fetch("average_price")),
      exchange_order_id:  ExchangeOrderID.from_dynamic!(d.fetch("exchange_order_id")),
      exchange_timestamp: ExchangeOrderID.from_dynamic!(d.fetch("exchange_timestamp")),
      folio:              Amount.from_dynamic!(d.fetch("folio")),
      fund:               Amount.from_dynamic!(d.fetch("fund")),
      last_price:         Amount.from_dynamic!(d.fetch("last_price")),
      last_price_date:    LastPriceDate.from_dynamic!(d.fetch("last_price_date")),
      order_id:           LastPriceDate.from_dynamic!(d.fetch("order_id")),
      order_timestamp:    LastPriceDate.from_dynamic!(d.fetch("order_timestamp")),
      placed_by:          Amount.from_dynamic!(d.fetch("placed_by")),
      purchase_type:      Amount.from_dynamic!(d.fetch("purchase_type")),
      quantity:           Amount.from_dynamic!(d.fetch("quantity")),
      settlement_id:      ExchangeOrderID.from_dynamic!(d.fetch("settlement_id")),
      status:             Amount.from_dynamic!(d.fetch("status")),
      status_message:     Amount.from_dynamic!(d.fetch("status_message")),
      tag:                Tag.from_dynamic!(d.fetch("tag")),
      tradingsymbol:      Amount.from_dynamic!(d.fetch("tradingsymbol")),
      transaction_type:   Amount.from_dynamic!(d.fetch("transaction_type")),
      variety:            Amount.from_dynamic!(d.fetch("variety")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "amount"             => @amount.to_dynamic,
      "average_price"      => @average_price.to_dynamic,
      "exchange_order_id"  => @exchange_order_id.to_dynamic,
      "exchange_timestamp" => @exchange_timestamp.to_dynamic,
      "folio"              => @folio.to_dynamic,
      "fund"               => @fund.to_dynamic,
      "last_price"         => @last_price.to_dynamic,
      "last_price_date"    => @last_price_date.to_dynamic,
      "order_id"           => @order_id.to_dynamic,
      "order_timestamp"    => @order_timestamp.to_dynamic,
      "placed_by"          => @placed_by.to_dynamic,
      "purchase_type"      => @purchase_type.to_dynamic,
      "quantity"           => @quantity.to_dynamic,
      "settlement_id"      => @settlement_id.to_dynamic,
      "status"             => @status.to_dynamic,
      "status_message"     => @status_message.to_dynamic,
      "tag"                => @tag.to_dynamic,
      "tradingsymbol"      => @tradingsymbol.to_dynamic,
      "transaction_type"   => @transaction_type.to_dynamic,
      "variety"            => @variety.to_dynamic,
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

class MFOrdersProperties < Dry::Struct
  attribute :data,   DataClass
  attribute :status, Amount

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   DataClass.from_dynamic!(d.fetch("data")),
      status: Amount.from_dynamic!(d.fetch("status")),
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

class MFOrdersClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            MFOrdersProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :mf_orders_class_type,  Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            MFOrdersProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      mf_orders_class_type:  d.fetch("type"),
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
      "type"                 => @mf_orders_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :datum,     Datum
  attribute :mf_orders, MFOrdersClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      datum:     Datum.from_dynamic!(d.fetch("Datum")),
      mf_orders: MFOrdersClass.from_dynamic!(d.fetch("MFOrders")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Datum"    => @datum.to_dynamic,
      "MFOrders" => @mf_orders.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MFOrders < Dry::Struct
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
