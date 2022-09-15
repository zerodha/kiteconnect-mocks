# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   mf_orders_info = MFOrdersInfo.from_json! "{…}"
#   puts mf_orders_info.definitions.mf_orders_info.required.first
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
  Type   = Strict::String.enum("integer", "null", "number", "string")
end

module Type
  Integer = "integer"
  Null    = "null"
  Number  = "number"
  String  = "string"
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
  attribute :last_price_date_format, Types::String
  attribute :last_price_date_type,   Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      last_price_date_format: d.fetch("format"),
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

class DataProperties < Dry::Struct
  attribute :amount,             Amount
  attribute :average_price,      Amount
  attribute :exchange_order_id,  Amount
  attribute :exchange_timestamp, Amount
  attribute :folio,              Amount
  attribute :fund,               Amount
  attribute :last_price,         Amount
  attribute :last_price_date,    LastPriceDate
  attribute :order_id,           LastPriceDate
  attribute :order_timestamp,    LastPriceDate
  attribute :placed_by,          Amount
  attribute :purchase_type,      Amount
  attribute :quantity,           Amount
  attribute :settlement_id,      Amount
  attribute :status,             Amount
  attribute :status_message,     Amount
  attribute :tag,                Amount
  attribute :tradingsymbol,      Amount
  attribute :transaction_type,   Amount
  attribute :variety,            Amount

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      amount:             Amount.from_dynamic!(d.fetch("amount")),
      average_price:      Amount.from_dynamic!(d.fetch("average_price")),
      exchange_order_id:  Amount.from_dynamic!(d.fetch("exchange_order_id")),
      exchange_timestamp: Amount.from_dynamic!(d.fetch("exchange_timestamp")),
      folio:              Amount.from_dynamic!(d.fetch("folio")),
      fund:               Amount.from_dynamic!(d.fetch("fund")),
      last_price:         Amount.from_dynamic!(d.fetch("last_price")),
      last_price_date:    LastPriceDate.from_dynamic!(d.fetch("last_price_date")),
      order_id:           LastPriceDate.from_dynamic!(d.fetch("order_id")),
      order_timestamp:    LastPriceDate.from_dynamic!(d.fetch("order_timestamp")),
      placed_by:          Amount.from_dynamic!(d.fetch("placed_by")),
      purchase_type:      Amount.from_dynamic!(d.fetch("purchase_type")),
      quantity:           Amount.from_dynamic!(d.fetch("quantity")),
      settlement_id:      Amount.from_dynamic!(d.fetch("settlement_id")),
      status:             Amount.from_dynamic!(d.fetch("status")),
      status_message:     Amount.from_dynamic!(d.fetch("status_message")),
      tag:                Amount.from_dynamic!(d.fetch("tag")),
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

class PropertiesData < Dry::Struct
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

class MFOrdersInfoProperties < Dry::Struct
  attribute :data,   PropertiesData
  attribute :status, Amount

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   PropertiesData.from_dynamic!(d.fetch("data")),
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

class MFOrdersInfoClass < Dry::Struct
  attribute :additional_properties,     Types::Bool
  attribute :properties,                MFOrdersInfoProperties
  attribute :required,                  Types.Array(Types::String)
  attribute :title,                     Types::String
  attribute :mf_orders_info_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:     d.fetch("additionalProperties"),
      properties:                MFOrdersInfoProperties.from_dynamic!(d.fetch("properties")),
      required:                  d.fetch("required"),
      title:                     d.fetch("title"),
      mf_orders_info_class_type: d.fetch("type"),
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
      "type"                 => @mf_orders_info_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :data,           DataClass
  attribute :mf_orders_info, MFOrdersInfoClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:           DataClass.from_dynamic!(d.fetch("Data")),
      mf_orders_info: MFOrdersInfoClass.from_dynamic!(d.fetch("MFOrdersInfo")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Data"         => @data.to_dynamic,
      "MFOrdersInfo" => @mf_orders_info.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MFOrdersInfo < Dry::Struct
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
