# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   order_info = OrderInfo.from_json! "{…}"
#   puts order_info.definitions.order_info.required.first
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

class ExchangeOrderID < Dry::Struct
  attribute :any_of, Types.Array(AveragePrice)

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      any_of: d.fetch("anyOf").map { |x| AveragePrice.from_dynamic!(x) },
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

class OrderTimestamp < Dry::Struct
  attribute :order_timestamp_format, Types::String.optional
  attribute :order_timestamp_type,   Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      order_timestamp_format: d["format"],
      order_timestamp_type:   d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "format" => @order_timestamp_format,
      "type"   => @order_timestamp_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class ExchangeTimestamp < Dry::Struct
  attribute :any_of, Types.Array(OrderTimestamp)

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      any_of: d.fetch("anyOf").map { |x| OrderTimestamp.from_dynamic!(x) },
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
  attribute :average_price,      AveragePrice
  attribute :cancelled_quantity, AveragePrice
  attribute :disclosed_quantity, AveragePrice
  attribute :exchange,           AveragePrice
  attribute :exchange_order_id,  ExchangeOrderID
  attribute :exchange_timestamp, ExchangeTimestamp
  attribute :filled_quantity,    AveragePrice
  attribute :instrument_token,   AveragePrice
  attribute :order_id,           AveragePrice
  attribute :order_timestamp,    OrderTimestamp
  attribute :order_type,         AveragePrice
  attribute :parent_order_id,    AveragePrice
  attribute :pending_quantity,   AveragePrice
  attribute :placed_by,          AveragePrice
  attribute :price,              AveragePrice
  attribute :product,            AveragePrice
  attribute :quantity,           AveragePrice
  attribute :status,             AveragePrice
  attribute :status_message,     AveragePrice
  attribute :tag,                AveragePrice
  attribute :tradingsymbol,      AveragePrice
  attribute :transaction_type,   AveragePrice
  attribute :trigger_price,      AveragePrice
  attribute :validity,           AveragePrice
  attribute :variety,            AveragePrice

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      average_price:      AveragePrice.from_dynamic!(d.fetch("average_price")),
      cancelled_quantity: AveragePrice.from_dynamic!(d.fetch("cancelled_quantity")),
      disclosed_quantity: AveragePrice.from_dynamic!(d.fetch("disclosed_quantity")),
      exchange:           AveragePrice.from_dynamic!(d.fetch("exchange")),
      exchange_order_id:  ExchangeOrderID.from_dynamic!(d.fetch("exchange_order_id")),
      exchange_timestamp: ExchangeTimestamp.from_dynamic!(d.fetch("exchange_timestamp")),
      filled_quantity:    AveragePrice.from_dynamic!(d.fetch("filled_quantity")),
      instrument_token:   AveragePrice.from_dynamic!(d.fetch("instrument_token")),
      order_id:           AveragePrice.from_dynamic!(d.fetch("order_id")),
      order_timestamp:    OrderTimestamp.from_dynamic!(d.fetch("order_timestamp")),
      order_type:         AveragePrice.from_dynamic!(d.fetch("order_type")),
      parent_order_id:    AveragePrice.from_dynamic!(d.fetch("parent_order_id")),
      pending_quantity:   AveragePrice.from_dynamic!(d.fetch("pending_quantity")),
      placed_by:          AveragePrice.from_dynamic!(d.fetch("placed_by")),
      price:              AveragePrice.from_dynamic!(d.fetch("price")),
      product:            AveragePrice.from_dynamic!(d.fetch("product")),
      quantity:           AveragePrice.from_dynamic!(d.fetch("quantity")),
      status:             AveragePrice.from_dynamic!(d.fetch("status")),
      status_message:     AveragePrice.from_dynamic!(d.fetch("status_message")),
      tag:                AveragePrice.from_dynamic!(d.fetch("tag")),
      tradingsymbol:      AveragePrice.from_dynamic!(d.fetch("tradingsymbol")),
      transaction_type:   AveragePrice.from_dynamic!(d.fetch("transaction_type")),
      trigger_price:      AveragePrice.from_dynamic!(d.fetch("trigger_price")),
      validity:           AveragePrice.from_dynamic!(d.fetch("validity")),
      variety:            AveragePrice.from_dynamic!(d.fetch("variety")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "average_price"      => @average_price.to_dynamic,
      "cancelled_quantity" => @cancelled_quantity.to_dynamic,
      "disclosed_quantity" => @disclosed_quantity.to_dynamic,
      "exchange"           => @exchange.to_dynamic,
      "exchange_order_id"  => @exchange_order_id.to_dynamic,
      "exchange_timestamp" => @exchange_timestamp.to_dynamic,
      "filled_quantity"    => @filled_quantity.to_dynamic,
      "instrument_token"   => @instrument_token.to_dynamic,
      "order_id"           => @order_id.to_dynamic,
      "order_timestamp"    => @order_timestamp.to_dynamic,
      "order_type"         => @order_type.to_dynamic,
      "parent_order_id"    => @parent_order_id.to_dynamic,
      "pending_quantity"   => @pending_quantity.to_dynamic,
      "placed_by"          => @placed_by.to_dynamic,
      "price"              => @price.to_dynamic,
      "product"            => @product.to_dynamic,
      "quantity"           => @quantity.to_dynamic,
      "status"             => @status.to_dynamic,
      "status_message"     => @status_message.to_dynamic,
      "tag"                => @tag.to_dynamic,
      "tradingsymbol"      => @tradingsymbol.to_dynamic,
      "transaction_type"   => @transaction_type.to_dynamic,
      "trigger_price"      => @trigger_price.to_dynamic,
      "validity"           => @validity.to_dynamic,
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

class OrderInfoProperties < Dry::Struct
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

class OrderInfoClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            OrderInfoProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :order_info_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            OrderInfoProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      order_info_class_type: d.fetch("type"),
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
      "type"                 => @order_info_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :datum,      Datum
  attribute :order_info, OrderInfoClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      datum:      Datum.from_dynamic!(d.fetch("Datum")),
      order_info: OrderInfoClass.from_dynamic!(d.fetch("OrderInfo")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Datum"     => @datum.to_dynamic,
      "OrderInfo" => @order_info.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OrderInfo < Dry::Struct
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
