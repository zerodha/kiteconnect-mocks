# This code may look unusually verbose for Ruby (and it is), but
# it performs some subtle and complex validation of JSON data.
#
# To parse this JSON, add 'dry-struct' and 'dry-types' gems, then do:
#
#   mf_sips = MFSips.from_json! "{…}"
#   puts mf_sips.definitions.mf_sips.required.first
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

class CompletedInstalments < Dry::Struct
  attribute :completed_instalments_type, Types::Type

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      completed_instalments_type: d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "type" => @completed_instalments_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Created < Dry::Struct
  attribute :created_format, Types::String.optional
  attribute :created_type,   Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      created_format: d["format"],
      created_type:   d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "format" => @created_format,
      "type"   => @created_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class SIPRegNum < Dry::Struct
  attribute :any_of, Types.Array(Created)

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      any_of: d.fetch("anyOf").map { |x| Created.from_dynamic!(x) },
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

class StepUp < Dry::Struct
  attribute :additional_properties, CompletedInstalments
  attribute :step_up_type,          Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: CompletedInstalments.from_dynamic!(d.fetch("additionalProperties")),
      step_up_type:          d.fetch("type"),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "additionalProperties" => @additional_properties.to_dynamic,
      "type"                 => @step_up_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class DatumProperties < Dry::Struct
  attribute :completed_instalments, CompletedInstalments
  attribute :created,               Created
  attribute :dividend_type,         CompletedInstalments
  attribute :frequency,             CompletedInstalments
  attribute :fund,                  CompletedInstalments
  attribute :instalment_amount,     CompletedInstalments
  attribute :instalment_day,        CompletedInstalments
  attribute :instalments,           CompletedInstalments
  attribute :last_instalment,       Created
  attribute :next_instalment,       Created
  attribute :pending_instalments,   CompletedInstalments
  attribute :sip_id,                CompletedInstalments
  attribute :sip_reg_num,           SIPRegNum
  attribute :sip_type,              CompletedInstalments
  attribute :status,                CompletedInstalments
  attribute :step_up,               StepUp
  attribute :tag,                   CompletedInstalments
  attribute :tradingsymbol,         CompletedInstalments
  attribute :transaction_type,      CompletedInstalments
  attribute :trigger_price,         CompletedInstalments

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      completed_instalments: CompletedInstalments.from_dynamic!(d.fetch("completed_instalments")),
      created:               Created.from_dynamic!(d.fetch("created")),
      dividend_type:         CompletedInstalments.from_dynamic!(d.fetch("dividend_type")),
      frequency:             CompletedInstalments.from_dynamic!(d.fetch("frequency")),
      fund:                  CompletedInstalments.from_dynamic!(d.fetch("fund")),
      instalment_amount:     CompletedInstalments.from_dynamic!(d.fetch("instalment_amount")),
      instalment_day:        CompletedInstalments.from_dynamic!(d.fetch("instalment_day")),
      instalments:           CompletedInstalments.from_dynamic!(d.fetch("instalments")),
      last_instalment:       Created.from_dynamic!(d.fetch("last_instalment")),
      next_instalment:       Created.from_dynamic!(d.fetch("next_instalment")),
      pending_instalments:   CompletedInstalments.from_dynamic!(d.fetch("pending_instalments")),
      sip_id:                CompletedInstalments.from_dynamic!(d.fetch("sip_id")),
      sip_reg_num:           SIPRegNum.from_dynamic!(d.fetch("sip_reg_num")),
      sip_type:              CompletedInstalments.from_dynamic!(d.fetch("sip_type")),
      status:                CompletedInstalments.from_dynamic!(d.fetch("status")),
      step_up:               StepUp.from_dynamic!(d.fetch("step_up")),
      tag:                   CompletedInstalments.from_dynamic!(d.fetch("tag")),
      tradingsymbol:         CompletedInstalments.from_dynamic!(d.fetch("tradingsymbol")),
      transaction_type:      CompletedInstalments.from_dynamic!(d.fetch("transaction_type")),
      trigger_price:         CompletedInstalments.from_dynamic!(d.fetch("trigger_price")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "completed_instalments" => @completed_instalments.to_dynamic,
      "created"               => @created.to_dynamic,
      "dividend_type"         => @dividend_type.to_dynamic,
      "frequency"             => @frequency.to_dynamic,
      "fund"                  => @fund.to_dynamic,
      "instalment_amount"     => @instalment_amount.to_dynamic,
      "instalment_day"        => @instalment_day.to_dynamic,
      "instalments"           => @instalments.to_dynamic,
      "last_instalment"       => @last_instalment.to_dynamic,
      "next_instalment"       => @next_instalment.to_dynamic,
      "pending_instalments"   => @pending_instalments.to_dynamic,
      "sip_id"                => @sip_id.to_dynamic,
      "sip_reg_num"           => @sip_reg_num.to_dynamic,
      "sip_type"              => @sip_type.to_dynamic,
      "status"                => @status.to_dynamic,
      "step_up"               => @step_up.to_dynamic,
      "tag"                   => @tag.to_dynamic,
      "tradingsymbol"         => @tradingsymbol.to_dynamic,
      "transaction_type"      => @transaction_type.to_dynamic,
      "trigger_price"         => @trigger_price.to_dynamic,
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

class MFSipsProperties < Dry::Struct
  attribute :data, DataClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      data: DataClass.from_dynamic!(d.fetch("data")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "data" => @data.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MFSipsClass < Dry::Struct
  attribute :additional_properties, Types::Bool
  attribute :properties,            MFSipsProperties
  attribute :required,              Types.Array(Types::String)
  attribute :title,                 Types::String
  attribute :mf_sips_class_type,    Types::String

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      additional_properties: d.fetch("additionalProperties"),
      properties:            MFSipsProperties.from_dynamic!(d.fetch("properties")),
      required:              d.fetch("required"),
      title:                 d.fetch("title"),
      mf_sips_class_type:    d.fetch("type"),
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
      "type"                 => @mf_sips_class_type,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class Definitions < Dry::Struct
  attribute :datum,   Datum
  attribute :mf_sips, MFSipsClass

  def self.from_dynamic!(d)
    d = Types::Hash[d]
    new(
      datum:   Datum.from_dynamic!(d.fetch("Datum")),
      mf_sips: MFSipsClass.from_dynamic!(d.fetch("MFSips")),
    )
  end

  def self.from_json!(json)
    from_dynamic!(JSON.parse(json))
  end

  def to_dynamic
    {
      "Datum"  => @datum.to_dynamic,
      "MFSips" => @mf_sips.to_dynamic,
    }
  end

  def to_json(options = nil)
    JSON.generate(to_dynamic, options)
  end
end

class MFSips < Dry::Struct
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
