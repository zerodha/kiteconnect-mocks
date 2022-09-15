# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   postback = Postback.from_json! "{…}"
#   puts postback.definitions.postback.required.first
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
  Type   = Strict::String.enum("integer", "null", "string")
end

class Meta < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :title,                 Types::String
  attribute :meta_type,             Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      title:                 d.fetch("title"),
      meta_type:             d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "additionalProperties" => @additional_properties,
      "title"                => @title,
      "type"                 => @meta_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

module Type
  Integer = "integer"
  Null    = "null"
  String  = "string"
end

class AppID < Dry::Struct
  attribute :app_id_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      app_id_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @app_id_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Timestamp < Dry::Struct
  attribute :timestamp_format, Types::String
  attribute :timestamp_type,   Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      timestamp_format: d.fetch("format"),
      timestamp_type:   d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "format" => @timestamp_format,
      "type"   => @timestamp_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MetaClass < Dry::Struct
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

class Properties < Dry::Struct
  attribute :app_id,                    AppID
  attribute :average_price,             AppID
  attribute :cancelled_quantity,        AppID
  attribute :checksum,                  AppID
  attribute :disclosed_quantity,        AppID
  attribute :exchange,                  AppID
  attribute :exchange_order_id,         AppID
  attribute :exchange_timestamp,        Timestamp
  attribute :exchange_update_timestamp, Timestamp
  attribute :filled_quantity,           AppID
  attribute :guid,                      AppID
  attribute :instrument_token,          AppID
  attribute :market_protection,         AppID
  attribute :properties_meta,           MetaClass
  attribute :order_id,                  AppID
  attribute :order_timestamp,           Timestamp
  attribute :order_type,                AppID
  attribute :parent_order_id,           AppID
  attribute :pending_quantity,          AppID
  attribute :placed_by,                 AppID
  attribute :price,                     AppID
  attribute :product,                   AppID
  attribute :quantity,                  AppID
  attribute :status,                    AppID
  attribute :status_message,            AppID
  attribute :status_message_raw,        AppID
  attribute :tag,                       AppID
  attribute :tradingsymbol,             AppID
  attribute :transaction_type,          AppID
  attribute :trigger_price,             AppID
  attribute :unfilled_quantity,         AppID
  attribute :user_id,                   AppID
  attribute :validity,                  AppID
  attribute :variety,                   AppID

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      app_id:                    AppID.from_dynamic!(d.fetch("app_id")),
      average_price:             AppID.from_dynamic!(d.fetch("average_price")),
      cancelled_quantity:        AppID.from_dynamic!(d.fetch("cancelled_quantity")),
      checksum:                  AppID.from_dynamic!(d.fetch("checksum")),
      disclosed_quantity:        AppID.from_dynamic!(d.fetch("disclosed_quantity")),
      exchange:                  AppID.from_dynamic!(d.fetch("exchange")),
      exchange_order_id:         AppID.from_dynamic!(d.fetch("exchange_order_id")),
      exchange_timestamp:        Timestamp.from_dynamic!(d.fetch("exchange_timestamp")),
      exchange_update_timestamp: Timestamp.from_dynamic!(d.fetch("exchange_update_timestamp")),
      filled_quantity:           AppID.from_dynamic!(d.fetch("filled_quantity")),
      guid:                      AppID.from_dynamic!(d.fetch("guid")),
      instrument_token:          AppID.from_dynamic!(d.fetch("instrument_token")),
      market_protection:         AppID.from_dynamic!(d.fetch("market_protection")),
      properties_meta:           MetaClass.from_dynamic!(d.fetch("meta")),
      order_id:                  AppID.from_dynamic!(d.fetch("order_id")),
      order_timestamp:           Timestamp.from_dynamic!(d.fetch("order_timestamp")),
      order_type:                AppID.from_dynamic!(d.fetch("order_type")),
      parent_order_id:           AppID.from_dynamic!(d.fetch("parent_order_id")),
      pending_quantity:          AppID.from_dynamic!(d.fetch("pending_quantity")),
      placed_by:                 AppID.from_dynamic!(d.fetch("placed_by")),
      price:                     AppID.from_dynamic!(d.fetch("price")),
      product:                   AppID.from_dynamic!(d.fetch("product")),
      quantity:                  AppID.from_dynamic!(d.fetch("quantity")),
      status:                    AppID.from_dynamic!(d.fetch("status")),
      status_message:            AppID.from_dynamic!(d.fetch("status_message")),
      status_message_raw:        AppID.from_dynamic!(d.fetch("status_message_raw")),
      tag:                       AppID.from_dynamic!(d.fetch("tag")),
      tradingsymbol:             AppID.from_dynamic!(d.fetch("tradingsymbol")),
      transaction_type:          AppID.from_dynamic!(d.fetch("transaction_type")),
      trigger_price:             AppID.from_dynamic!(d.fetch("trigger_price")),
      unfilled_quantity:         AppID.from_dynamic!(d.fetch("unfilled_quantity")),
      user_id:                   AppID.from_dynamic!(d.fetch("user_id")),
      validity:                  AppID.from_dynamic!(d.fetch("validity")),
      variety:                   AppID.from_dynamic!(d.fetch("variety")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "app_id"                    => @app_id.to_dynamic,
      "average_price"             => @average_price.to_dynamic,
      "cancelled_quantity"        => @cancelled_quantity.to_dynamic,
      "checksum"                  => @checksum.to_dynamic,
      "disclosed_quantity"        => @disclosed_quantity.to_dynamic,
      "exchange"                  => @exchange.to_dynamic,
      "exchange_order_id"         => @exchange_order_id.to_dynamic,
      "exchange_timestamp"        => @exchange_timestamp.to_dynamic,
      "exchange_update_timestamp" => @exchange_update_timestamp.to_dynamic,
      "filled_quantity"           => @filled_quantity.to_dynamic,
      "guid"                      => @guid.to_dynamic,
      "instrument_token"          => @instrument_token.to_dynamic,
      "market_protection"         => @market_protection.to_dynamic,
      "meta"                      => @properties_meta.to_dynamic,
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
      "tradingsymbol"             => @tradingsymbol.to_dynamic,
      "transaction_type"          => @transaction_type.to_dynamic,
      "trigger_price"             => @trigger_price.to_dynamic,
      "unfilled_quantity"         => @unfilled_quantity.to_dynamic,
      "user_id"                   => @user_id.to_dynamic,
      "validity"                  => @validity.to_dynamic,
      "variety"                   => @variety.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class PostbackClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            Properties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :postback_class_type,   Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            Properties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      postback_class_type:   d.fetch("type"),
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
      "type"                 => @postback_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :definitions_meta, Meta
  attribute :postback,         PostbackClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      definitions_meta: Meta.from_dynamic!(d.fetch("Meta")),
      postback:         PostbackClass.from_dynamic!(d.fetch("Postback")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Meta"     => @definitions_meta.to_dynamic,
      "Postback" => @postback.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Postback < Dry::Struct
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
