# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   gtt_get_orders = GttGetOrders.from_json! "{…}"
#   puts gtt_get_orders.definitions.result.required.first
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

class Exchange < Dry::Struct
  attribute :exchange_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      exchange_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @exchange_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class TriggerValues < Dry::Struct
  attribute :items,               Exchange
  attribute :trigger_values_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:               Exchange.from_dynamic!(d.fetch("items")),
      trigger_values_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "items" => @items.to_dynamic,
      "type"  => @trigger_values_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class ConditionProperties < Dry::Struct
  attribute :exchange,         Exchange
  attribute :instrument_token, Exchange
  attribute :last_price,       Exchange
  attribute :tradingsymbol,    Exchange
  attribute :trigger_values,   TriggerValues

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      exchange:         Exchange.from_dynamic!(d.fetch("exchange")),
      instrument_token: Exchange.from_dynamic!(d.fetch("instrument_token")),
      last_price:       Exchange.from_dynamic!(d.fetch("last_price")),
      tradingsymbol:    Exchange.from_dynamic!(d.fetch("tradingsymbol")),
      trigger_values:   TriggerValues.from_dynamic!(d.fetch("trigger_values")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "exchange"         => @exchange.to_dynamic,
      "instrument_token" => @instrument_token.to_dynamic,
      "last_price"       => @last_price.to_dynamic,
      "tradingsymbol"    => @tradingsymbol.to_dynamic,
      "trigger_values"   => @trigger_values.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Condition < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            ConditionProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :condition_type,        Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            ConditionProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      condition_type:        d.fetch("type"),
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
      "type"                 => @condition_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class ConditionClass < Dry::Struct
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

class CreatedAt < Dry::Struct
  attribute :created_at_format, Types::String
  attribute :created_at_type,   Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      created_at_format: d.fetch("format"),
      created_at_type:   d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "format" => @created_at_format,
      "type"   => @created_at_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class AnyOf < Dry::Struct
  attribute :ref,         Types::String.optional
  attribute :any_of_type, Types::Type.optional

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      ref:         d["$ref"],
      any_of_type: d["type"],
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "$ref" => @ref,
      "type" => @any_of_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Meta < Dry::Struct
  attribute :any_of, Types.Array(AnyOf)

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      any_of: d.fetch("anyOf").map { |x| AnyOf.from_dynamic!(x) },
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

class Orders < Dry::Struct
  attribute :items,       ConditionClass
  attribute :orders_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      items:       ConditionClass.from_dynamic!(d.fetch("items")),
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

class DatumProperties < Dry::Struct
  attribute :condition,             ConditionClass
  attribute :created_at,            CreatedAt
  attribute :expires_at,            CreatedAt
  attribute :id,                    Exchange
  attribute :datum_properties_meta, Meta
  attribute :orders,                Orders
  attribute :parent_trigger,        Exchange
  attribute :status,                Exchange
  attribute :datum_properties_type, Exchange
  attribute :updated_at,            CreatedAt
  attribute :user_id,               Exchange

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      condition:             ConditionClass.from_dynamic!(d.fetch("condition")),
      created_at:            CreatedAt.from_dynamic!(d.fetch("created_at")),
      expires_at:            CreatedAt.from_dynamic!(d.fetch("expires_at")),
      id:                    Exchange.from_dynamic!(d.fetch("id")),
      datum_properties_meta: Meta.from_dynamic!(d.fetch("meta")),
      orders:                Orders.from_dynamic!(d.fetch("orders")),
      parent_trigger:        Exchange.from_dynamic!(d.fetch("parent_trigger")),
      status:                Exchange.from_dynamic!(d.fetch("status")),
      datum_properties_type: Exchange.from_dynamic!(d.fetch("type")),
      updated_at:            CreatedAt.from_dynamic!(d.fetch("updated_at")),
      user_id:               Exchange.from_dynamic!(d.fetch("user_id")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "condition"      => @condition.to_dynamic,
      "created_at"     => @created_at.to_dynamic,
      "expires_at"     => @expires_at.to_dynamic,
      "id"             => @id.to_dynamic,
      "meta"           => @datum_properties_meta.to_dynamic,
      "orders"         => @orders.to_dynamic,
      "parent_trigger" => @parent_trigger.to_dynamic,
      "status"         => @status.to_dynamic,
      "type"           => @datum_properties_type.to_dynamic,
      "updated_at"     => @updated_at.to_dynamic,
      "user_id"        => @user_id.to_dynamic,
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

class MetaClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :title,                 Types::String
  attribute :meta_class_type,       Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
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
      "title"                => @title,
      "type"                 => @meta_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class GttGetOrdersProperties < Dry::Struct
  attribute :data,   Orders
  attribute :status, Exchange

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data:   Orders.from_dynamic!(d.fetch("data")),
      status: Exchange.from_dynamic!(d.fetch("status")),
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

class GttGetOrdersClass < Dry::Struct
  attribute :additional_properties,     Types::Bool
  attribute :properties,                GttGetOrdersProperties
  attribute :required,                  Types.Array(Types::String)
  attribute :title,                     Types::String
  attribute :gtt_get_orders_class_type, Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties:     d.fetch("additionalProperties"),
      properties:                GttGetOrdersProperties.from_dynamic!(d.fetch("properties")),
      required:                  d.fetch("required"),
      title:                     d.fetch("title"),
      gtt_get_orders_class_type: d.fetch("type"),
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
      "type"                 => @gtt_get_orders_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OrderProperties < Dry::Struct
  attribute :exchange,         Exchange
  attribute :order_type,       Exchange
  attribute :price,            Exchange
  attribute :product,          Exchange
  attribute :quantity,         Exchange
  attribute :result,           Meta
  attribute :tradingsymbol,    Exchange
  attribute :transaction_type, Exchange

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      exchange:         Exchange.from_dynamic!(d.fetch("exchange")),
      order_type:       Exchange.from_dynamic!(d.fetch("order_type")),
      price:            Exchange.from_dynamic!(d.fetch("price")),
      product:          Exchange.from_dynamic!(d.fetch("product")),
      quantity:         Exchange.from_dynamic!(d.fetch("quantity")),
      result:           Meta.from_dynamic!(d.fetch("result")),
      tradingsymbol:    Exchange.from_dynamic!(d.fetch("tradingsymbol")),
      transaction_type: Exchange.from_dynamic!(d.fetch("transaction_type")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "exchange"         => @exchange.to_dynamic,
      "order_type"       => @order_type.to_dynamic,
      "price"            => @price.to_dynamic,
      "product"          => @product.to_dynamic,
      "quantity"         => @quantity.to_dynamic,
      "result"           => @result.to_dynamic,
      "tradingsymbol"    => @tradingsymbol.to_dynamic,
      "transaction_type" => @transaction_type.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Order < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            OrderProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :order_type,            Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            OrderProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      order_type:            d.fetch("type"),
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
      "type"                 => @order_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OrderResultProperties < Dry::Struct
  attribute :order_id,         Exchange
  attribute :rejection_reason, Exchange
  attribute :status,           Exchange

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      order_id:         Exchange.from_dynamic!(d.fetch("order_id")),
      rejection_reason: Exchange.from_dynamic!(d.fetch("rejection_reason")),
      status:           Exchange.from_dynamic!(d.fetch("status")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "order_id"         => @order_id.to_dynamic,
      "rejection_reason" => @rejection_reason.to_dynamic,
      "status"           => @status.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class OrderResult < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            OrderResultProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :order_result_type,     Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            OrderResultProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      order_result_type:     d.fetch("type"),
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
      "type"                 => @order_result_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class ResultProperties < Dry::Struct
  attribute :account_id,             Exchange
  attribute :exchange,               Exchange
  attribute :result_properties_meta, Exchange
  attribute :order_result,           ConditionClass
  attribute :order_type,             Exchange
  attribute :price,                  Exchange
  attribute :product,                Exchange
  attribute :quantity,               Exchange
  attribute :timestamp,              CreatedAt
  attribute :tradingsymbol,          Exchange
  attribute :transaction_type,       Exchange
  attribute :triggered_at,           Exchange
  attribute :validity,               Exchange

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      account_id:             Exchange.from_dynamic!(d.fetch("account_id")),
      exchange:               Exchange.from_dynamic!(d.fetch("exchange")),
      result_properties_meta: Exchange.from_dynamic!(d.fetch("meta")),
      order_result:           ConditionClass.from_dynamic!(d.fetch("order_result")),
      order_type:             Exchange.from_dynamic!(d.fetch("order_type")),
      price:                  Exchange.from_dynamic!(d.fetch("price")),
      product:                Exchange.from_dynamic!(d.fetch("product")),
      quantity:               Exchange.from_dynamic!(d.fetch("quantity")),
      timestamp:              CreatedAt.from_dynamic!(d.fetch("timestamp")),
      tradingsymbol:          Exchange.from_dynamic!(d.fetch("tradingsymbol")),
      transaction_type:       Exchange.from_dynamic!(d.fetch("transaction_type")),
      triggered_at:           Exchange.from_dynamic!(d.fetch("triggered_at")),
      validity:               Exchange.from_dynamic!(d.fetch("validity")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "account_id"       => @account_id.to_dynamic,
      "exchange"         => @exchange.to_dynamic,
      "meta"             => @result_properties_meta.to_dynamic,
      "order_result"     => @order_result.to_dynamic,
      "order_type"       => @order_type.to_dynamic,
      "price"            => @price.to_dynamic,
      "product"          => @product.to_dynamic,
      "quantity"         => @quantity.to_dynamic,
      "timestamp"        => @timestamp.to_dynamic,
      "tradingsymbol"    => @tradingsymbol.to_dynamic,
      "transaction_type" => @transaction_type.to_dynamic,
      "triggered_at"     => @triggered_at.to_dynamic,
      "validity"         => @validity.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Result < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            ResultProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :result_type,           Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            ResultProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      result_type:           d.fetch("type"),
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
      "type"                 => @result_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :condition,        Condition
  attribute :datum,            Datum
  attribute :gtt_get_orders,   GttGetOrdersClass
  attribute :definitions_meta, MetaClass
  attribute :order,            Order
  attribute :order_result,     OrderResult
  attribute :result,           Result

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      condition:        Condition.from_dynamic!(d.fetch("Condition")),
      datum:            Datum.from_dynamic!(d.fetch("Datum")),
      gtt_get_orders:   GttGetOrdersClass.from_dynamic!(d.fetch("GttGetOrders")),
      definitions_meta: MetaClass.from_dynamic!(d.fetch("Meta")),
      order:            Order.from_dynamic!(d.fetch("Order")),
      order_result:     OrderResult.from_dynamic!(d.fetch("OrderResult")),
      result:           Result.from_dynamic!(d.fetch("Result")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Condition"    => @condition.to_dynamic,
      "Datum"        => @datum.to_dynamic,
      "GttGetOrders" => @gtt_get_orders.to_dynamic,
      "Meta"         => @definitions_meta.to_dynamic,
      "Order"        => @order.to_dynamic,
      "OrderResult"  => @order_result.to_dynamic,
      "Result"       => @result.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class GttGetOrders < Dry::Struct
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
