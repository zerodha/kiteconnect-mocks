# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   orders = Orders.from_json! "{…}"
#   puts orders.definitions.orders.required.first
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
  Type   = Strict::String.enum("boolean", "integer", "null", "string")
end

module Type
  Boolean = "boolean"
  Integer = "integer"
  Null    = "null"
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

class Meta < Dry::Struct
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

class ExchangeETimestamp < Dry::Struct
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

class Tags < Dry::Struct
  attribute :items,     AveragePrice
  attribute :tags_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:     AveragePrice.from_dynamic!(d.fetch("items")),
      tags_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @tags_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DatumProperties < Dry::Struct
  attribute :average_price,             AveragePrice
  attribute :cancelled_quantity,        AveragePrice
  attribute :disclosed_quantity,        AveragePrice
  attribute :exchange,                  AveragePrice
  attribute :exchange_order_id,         ExchangeOrderID
  attribute :exchange_timestamp,        ExchangeETimestamp
  attribute :exchange_update_timestamp, ExchangeETimestamp
  attribute :filled_quantity,           AveragePrice
  attribute :guid,                      AveragePrice
  attribute :instrument_token,          AveragePrice
  attribute :market_protection,         AveragePrice
  attribute :datum_properties_meta,     Meta
  attribute :modified,                  AveragePrice
  attribute :order_id,                  AveragePrice
  attribute :order_timestamp,           OrderTimestamp
  attribute :order_type,                AveragePrice
  attribute :parent_order_id,           AveragePrice
  attribute :pending_quantity,          AveragePrice
  attribute :placed_by,                 AveragePrice
  attribute :price,                     AveragePrice
  attribute :product,                   AveragePrice
  attribute :quantity,                  AveragePrice
  attribute :status,                    AveragePrice
  attribute :status_message,            ExchangeOrderID
  attribute :status_message_raw,        ExchangeOrderID
  attribute :tag,                       ExchangeOrderID
  attribute :tags,                      Tags
  attribute :tradingsymbol,             AveragePrice
  attribute :transaction_type,          AveragePrice
  attribute :trigger_price,             AveragePrice
  attribute :validity,                  AveragePrice
  attribute :validity_ttl,              AveragePrice
  attribute :variety,                   AveragePrice

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      average_price:             AveragePrice.from_dynamic!(d.fetch("average_price")),
      cancelled_quantity:        AveragePrice.from_dynamic!(d.fetch("cancelled_quantity")),
      disclosed_quantity:        AveragePrice.from_dynamic!(d.fetch("disclosed_quantity")),
      exchange:                  AveragePrice.from_dynamic!(d.fetch("exchange")),
      exchange_order_id:         ExchangeOrderID.from_dynamic!(d.fetch("exchange_order_id")),
      exchange_timestamp:        ExchangeETimestamp.from_dynamic!(d.fetch("exchange_timestamp")),
      exchange_update_timestamp: ExchangeETimestamp.from_dynamic!(d.fetch("exchange_update_timestamp")),
      filled_quantity:           AveragePrice.from_dynamic!(d.fetch("filled_quantity")),
      guid:                      AveragePrice.from_dynamic!(d.fetch("guid")),
      instrument_token:          AveragePrice.from_dynamic!(d.fetch("instrument_token")),
      market_protection:         AveragePrice.from_dynamic!(d.fetch("market_protection")),
      datum_properties_meta:     Meta.from_dynamic!(d.fetch("meta")),
      modified:                  AveragePrice.from_dynamic!(d.fetch("modified")),
      order_id:                  AveragePrice.from_dynamic!(d.fetch("order_id")),
      order_timestamp:           OrderTimestamp.from_dynamic!(d.fetch("order_timestamp")),
      order_type:                AveragePrice.from_dynamic!(d.fetch("order_type")),
      parent_order_id:           AveragePrice.from_dynamic!(d.fetch("parent_order_id")),
      pending_quantity:          AveragePrice.from_dynamic!(d.fetch("pending_quantity")),
      placed_by:                 AveragePrice.from_dynamic!(d.fetch("placed_by")),
      price:                     AveragePrice.from_dynamic!(d.fetch("price")),
      product:                   AveragePrice.from_dynamic!(d.fetch("product")),
      quantity:                  AveragePrice.from_dynamic!(d.fetch("quantity")),
      status:                    AveragePrice.from_dynamic!(d.fetch("status")),
      status_message:            ExchangeOrderID.from_dynamic!(d.fetch("status_message")),
      status_message_raw:        ExchangeOrderID.from_dynamic!(d.fetch("status_message_raw")),
      tag:                       ExchangeOrderID.from_dynamic!(d.fetch("tag")),
      tags:                      Tags.from_dynamic!(d.fetch("tags")),
      tradingsymbol:             AveragePrice.from_dynamic!(d.fetch("tradingsymbol")),
      transaction_type:          AveragePrice.from_dynamic!(d.fetch("transaction_type")),
      trigger_price:             AveragePrice.from_dynamic!(d.fetch("trigger_price")),
      validity:                  AveragePrice.from_dynamic!(d.fetch("validity")),
      validity_ttl:              AveragePrice.from_dynamic!(d.fetch("validity_ttl")),
      variety:                   AveragePrice.from_dynamic!(d.fetch("variety")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "average_price"             => @average_price.to_dynamic,
      "cancelled_quantity"        => @cancelled_quantity.to_dynamic,
      "disclosed_quantity"        => @disclosed_quantity.to_dynamic,
      "exchange"                  => @exchange.to_dynamic,
      "exchange_order_id"         => @exchange_order_id.to_dynamic,
      "exchange_timestamp"        => @exchange_timestamp.to_dynamic,
      "exchange_update_timestamp" => @exchange_update_timestamp.to_dynamic,
      "filled_quantity"           => @filled_quantity.to_dynamic,
      "guid"                      => @guid.to_dynamic,
      "instrument_token"          => @instrument_token.to_dynamic,
      "market_protection"         => @market_protection.to_dynamic,
      "meta"                      => @datum_properties_meta.to_dynamic,
      "modified"                  => @modified.to_dynamic,
      "order_id"                  => @order_id.to_dynamic,
      "order_timestamp"           => @order_timestamp.to_dynamic,
      "order_type"                => @order_type.to_dynamic,
      "parent_order_id"           => @parent_order_id.to_dynamic,
      "pending_quantity"          => @pending_quantity.to_dynamic,
      "placed_by"                 => @placed_by.to_dynamic,
      "price"                     => @price.to_dynamic,
      "product"                   => @product.to_dynamic,
      "quantity"                  => @quantity.to_dynamic,
      "status"                    => @status.to_dynamic,
      "status_message"            => @status_message.to_dynamic,
      "status_message_raw"        => @status_message_raw.to_dynamic,
      "tag"                       => @tag.to_dynamic,
      "tags"                      => @tags.to_dynamic,
      "tradingsymbol"             => @tradingsymbol.to_dynamic,
      "transaction_type"          => @transaction_type.to_dynamic,
      "trigger_price"             => @trigger_price.to_dynamic,
      "validity"                  => @validity.to_dynamic,
      "validity_ttl"              => @validity_ttl.to_dynamic,
      "variety"                   => @variety.to_dynamic,
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

class MetaProperties < Dry::Struct
  attribute :iceberg, Meta

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      iceberg: Meta.from_dynamic!(d.fetch("iceberg")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "iceberg" => @iceberg.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MetaClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            MetaProperties
  attribute :required,              Types.Array(Types::Any)
  attribute :title,                 Types::String
  attribute :meta_class_type,       Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            MetaProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      meta_class_type:       d.fetch("type"),
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
      "type"                 => @meta_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class IcebergProperties < Dry::Struct
  attribute :leg,                AveragePrice
  attribute :leg_quantity,       AveragePrice
  attribute :legs,               AveragePrice
  attribute :remaining_quantity, AveragePrice
  attribute :total_quantity,     AveragePrice

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      leg:                AveragePrice.from_dynamic!(d.fetch("leg")),
      leg_quantity:       AveragePrice.from_dynamic!(d.fetch("leg_quantity")),
      legs:               AveragePrice.from_dynamic!(d.fetch("legs")),
      remaining_quantity: AveragePrice.from_dynamic!(d.fetch("remaining_quantity")),
      total_quantity:     AveragePrice.from_dynamic!(d.fetch("total_quantity")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "leg"                => @leg.to_dynamic,
      "leg_quantity"       => @leg_quantity.to_dynamic,
      "legs"               => @legs.to_dynamic,
      "remaining_quantity" => @remaining_quantity.to_dynamic,
      "total_quantity"     => @total_quantity.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Iceberg < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            IcebergProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :iceberg_type,          Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            IcebergProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      iceberg_type:          d.fetch("type"),
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
      "type"                 => @iceberg_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DataClass < Dry::Struct
  attribute :items,     Meta
  attribute :data_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:     Meta.from_dynamic!(d.fetch("items")),
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

class OrdersProperties < Dry::Struct
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

class OrdersClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            OrdersProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :orders_class_type,     Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            OrdersProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      orders_class_type:     d.fetch("type"),
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
      "type"                 => @orders_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :datum,            Datum
  attribute :iceberg,          Iceberg
  attribute :definitions_meta, MetaClass
  attribute :orders,           OrdersClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      datum:            Datum.from_dynamic!(d.fetch("Datum")),
      iceberg:          Iceberg.from_dynamic!(d.fetch("Iceberg")),
      definitions_meta: MetaClass.from_dynamic!(d.fetch("Meta")),
      orders:           OrdersClass.from_dynamic!(d.fetch("Orders")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Datum"   => @datum.to_dynamic,
      "Iceberg" => @iceberg.to_dynamic,
      "Meta"    => @definitions_meta.to_dynamic,
      "Orders"  => @orders.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Orders < Dry::Struct
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
