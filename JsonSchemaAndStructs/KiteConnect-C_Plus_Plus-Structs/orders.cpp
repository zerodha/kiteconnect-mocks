//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Orders data = nlohmann::json::parse(jsonString);

#pragma once

#include "json.hpp"

#include <boost/optional.hpp>
#include <stdexcept>
#include <regex>

#ifndef NLOHMANN_OPT_HELPER
#define NLOHMANN_OPT_HELPER
namespace nlohmann {
    template <typename T>
    struct adl_serializer<std::shared_ptr<T>> {
        static void to_json(json & j, const std::shared_ptr<T> & opt) {
            if (!opt) j = nullptr; else j = *opt;
        }

        static std::shared_ptr<T> from_json(const json & j) {
            if (j.is_null()) return std::unique_ptr<T>(); else return std::unique_ptr<T>(new T(j.get<T>()));
        }
    };
}
#endif

namespace quicktype {
    using nlohmann::json;

    inline json get_untyped(const json & j, const char * property) {
        if (j.find(property) != j.end()) {
            return j.at(property).get<json>();
        }
        return json();
    }

    inline json get_untyped(const json & j, std::string property) {
        return get_untyped(j, property.data());
    }

    template <typename T>
    inline std::shared_ptr<T> get_optional(const json & j, const char * property) {
        if (j.find(property) != j.end()) {
            return j.at(property).get<std::shared_ptr<T>>();
        }
        return std::shared_ptr<T>();
    }

    template <typename T>
    inline std::shared_ptr<T> get_optional(const json & j, std::string property) {
        return get_optional<T>(j, property.data());
    }

    enum class Type : int { BOOLEAN, INTEGER, STRING, TYPE_NULL };

    class AveragePrice {
        public:
        AveragePrice() = default;
        virtual ~AveragePrice() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class ExchangeOrderId {
        public:
        ExchangeOrderId() = default;
        virtual ~ExchangeOrderId() = default;

        private:
        std::vector<AveragePrice> any_of;

        public:
        const std::vector<AveragePrice> & get_any_of() const { return any_of; }
        std::vector<AveragePrice> & get_mutable_any_of() { return any_of; }
        void set_any_of(const std::vector<AveragePrice> & value) { this->any_of = value; }
    };

    class OrderTimestamp {
        public:
        OrderTimestamp() = default;
        virtual ~OrderTimestamp() = default;

        private:
        std::shared_ptr<std::string> format;
        Type type;

        public:
        std::shared_ptr<std::string> get_format() const { return format; }
        void set_format(std::shared_ptr<std::string> value) { this->format = value; }

        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class ExchangeETimestamp {
        public:
        ExchangeETimestamp() = default;
        virtual ~ExchangeETimestamp() = default;

        private:
        std::vector<OrderTimestamp> any_of;

        public:
        const std::vector<OrderTimestamp> & get_any_of() const { return any_of; }
        std::vector<OrderTimestamp> & get_mutable_any_of() { return any_of; }
        void set_any_of(const std::vector<OrderTimestamp> & value) { this->any_of = value; }
    };

    class Meta {
        public:
        Meta() = default;
        virtual ~Meta() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    class Tags {
        public:
        Tags() = default;
        virtual ~Tags() = default;

        private:
        AveragePrice items;
        std::string type;

        public:
        const AveragePrice & get_items() const { return items; }
        AveragePrice & get_mutable_items() { return items; }
        void set_items(const AveragePrice & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class DatumProperties {
        public:
        DatumProperties() = default;
        virtual ~DatumProperties() = default;

        private:
        AveragePrice average_price;
        AveragePrice cancelled_quantity;
        AveragePrice disclosed_quantity;
        AveragePrice exchange;
        ExchangeOrderId exchange_order_id;
        ExchangeETimestamp exchange_timestamp;
        ExchangeETimestamp exchange_update_timestamp;
        AveragePrice filled_quantity;
        AveragePrice guid;
        AveragePrice instrument_token;
        AveragePrice market_protection;
        Meta meta;
        AveragePrice modified;
        AveragePrice order_id;
        OrderTimestamp order_timestamp;
        AveragePrice order_type;
        AveragePrice parent_order_id;
        AveragePrice pending_quantity;
        AveragePrice placed_by;
        AveragePrice price;
        AveragePrice product;
        AveragePrice quantity;
        AveragePrice status;
        ExchangeOrderId status_message;
        ExchangeOrderId status_message_raw;
        ExchangeOrderId tag;
        Tags tags;
        AveragePrice tradingsymbol;
        AveragePrice transaction_type;
        AveragePrice trigger_price;
        AveragePrice validity;
        AveragePrice validity_ttl;
        AveragePrice variety;

        public:
        const AveragePrice & get_average_price() const { return average_price; }
        AveragePrice & get_mutable_average_price() { return average_price; }
        void set_average_price(const AveragePrice & value) { this->average_price = value; }

        const AveragePrice & get_cancelled_quantity() const { return cancelled_quantity; }
        AveragePrice & get_mutable_cancelled_quantity() { return cancelled_quantity; }
        void set_cancelled_quantity(const AveragePrice & value) { this->cancelled_quantity = value; }

        const AveragePrice & get_disclosed_quantity() const { return disclosed_quantity; }
        AveragePrice & get_mutable_disclosed_quantity() { return disclosed_quantity; }
        void set_disclosed_quantity(const AveragePrice & value) { this->disclosed_quantity = value; }

        const AveragePrice & get_exchange() const { return exchange; }
        AveragePrice & get_mutable_exchange() { return exchange; }
        void set_exchange(const AveragePrice & value) { this->exchange = value; }

        const ExchangeOrderId & get_exchange_order_id() const { return exchange_order_id; }
        ExchangeOrderId & get_mutable_exchange_order_id() { return exchange_order_id; }
        void set_exchange_order_id(const ExchangeOrderId & value) { this->exchange_order_id = value; }

        const ExchangeETimestamp & get_exchange_timestamp() const { return exchange_timestamp; }
        ExchangeETimestamp & get_mutable_exchange_timestamp() { return exchange_timestamp; }
        void set_exchange_timestamp(const ExchangeETimestamp & value) { this->exchange_timestamp = value; }

        const ExchangeETimestamp & get_exchange_update_timestamp() const { return exchange_update_timestamp; }
        ExchangeETimestamp & get_mutable_exchange_update_timestamp() { return exchange_update_timestamp; }
        void set_exchange_update_timestamp(const ExchangeETimestamp & value) { this->exchange_update_timestamp = value; }

        const AveragePrice & get_filled_quantity() const { return filled_quantity; }
        AveragePrice & get_mutable_filled_quantity() { return filled_quantity; }
        void set_filled_quantity(const AveragePrice & value) { this->filled_quantity = value; }

        const AveragePrice & get_guid() const { return guid; }
        AveragePrice & get_mutable_guid() { return guid; }
        void set_guid(const AveragePrice & value) { this->guid = value; }

        const AveragePrice & get_instrument_token() const { return instrument_token; }
        AveragePrice & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const AveragePrice & value) { this->instrument_token = value; }

        const AveragePrice & get_market_protection() const { return market_protection; }
        AveragePrice & get_mutable_market_protection() { return market_protection; }
        void set_market_protection(const AveragePrice & value) { this->market_protection = value; }

        const Meta & get_meta() const { return meta; }
        Meta & get_mutable_meta() { return meta; }
        void set_meta(const Meta & value) { this->meta = value; }

        const AveragePrice & get_modified() const { return modified; }
        AveragePrice & get_mutable_modified() { return modified; }
        void set_modified(const AveragePrice & value) { this->modified = value; }

        const AveragePrice & get_order_id() const { return order_id; }
        AveragePrice & get_mutable_order_id() { return order_id; }
        void set_order_id(const AveragePrice & value) { this->order_id = value; }

        const OrderTimestamp & get_order_timestamp() const { return order_timestamp; }
        OrderTimestamp & get_mutable_order_timestamp() { return order_timestamp; }
        void set_order_timestamp(const OrderTimestamp & value) { this->order_timestamp = value; }

        const AveragePrice & get_order_type() const { return order_type; }
        AveragePrice & get_mutable_order_type() { return order_type; }
        void set_order_type(const AveragePrice & value) { this->order_type = value; }

        const AveragePrice & get_parent_order_id() const { return parent_order_id; }
        AveragePrice & get_mutable_parent_order_id() { return parent_order_id; }
        void set_parent_order_id(const AveragePrice & value) { this->parent_order_id = value; }

        const AveragePrice & get_pending_quantity() const { return pending_quantity; }
        AveragePrice & get_mutable_pending_quantity() { return pending_quantity; }
        void set_pending_quantity(const AveragePrice & value) { this->pending_quantity = value; }

        const AveragePrice & get_placed_by() const { return placed_by; }
        AveragePrice & get_mutable_placed_by() { return placed_by; }
        void set_placed_by(const AveragePrice & value) { this->placed_by = value; }

        const AveragePrice & get_price() const { return price; }
        AveragePrice & get_mutable_price() { return price; }
        void set_price(const AveragePrice & value) { this->price = value; }

        const AveragePrice & get_product() const { return product; }
        AveragePrice & get_mutable_product() { return product; }
        void set_product(const AveragePrice & value) { this->product = value; }

        const AveragePrice & get_quantity() const { return quantity; }
        AveragePrice & get_mutable_quantity() { return quantity; }
        void set_quantity(const AveragePrice & value) { this->quantity = value; }

        const AveragePrice & get_status() const { return status; }
        AveragePrice & get_mutable_status() { return status; }
        void set_status(const AveragePrice & value) { this->status = value; }

        const ExchangeOrderId & get_status_message() const { return status_message; }
        ExchangeOrderId & get_mutable_status_message() { return status_message; }
        void set_status_message(const ExchangeOrderId & value) { this->status_message = value; }

        const ExchangeOrderId & get_status_message_raw() const { return status_message_raw; }
        ExchangeOrderId & get_mutable_status_message_raw() { return status_message_raw; }
        void set_status_message_raw(const ExchangeOrderId & value) { this->status_message_raw = value; }

        const ExchangeOrderId & get_tag() const { return tag; }
        ExchangeOrderId & get_mutable_tag() { return tag; }
        void set_tag(const ExchangeOrderId & value) { this->tag = value; }

        const Tags & get_tags() const { return tags; }
        Tags & get_mutable_tags() { return tags; }
        void set_tags(const Tags & value) { this->tags = value; }

        const AveragePrice & get_tradingsymbol() const { return tradingsymbol; }
        AveragePrice & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const AveragePrice & value) { this->tradingsymbol = value; }

        const AveragePrice & get_transaction_type() const { return transaction_type; }
        AveragePrice & get_mutable_transaction_type() { return transaction_type; }
        void set_transaction_type(const AveragePrice & value) { this->transaction_type = value; }

        const AveragePrice & get_trigger_price() const { return trigger_price; }
        AveragePrice & get_mutable_trigger_price() { return trigger_price; }
        void set_trigger_price(const AveragePrice & value) { this->trigger_price = value; }

        const AveragePrice & get_validity() const { return validity; }
        AveragePrice & get_mutable_validity() { return validity; }
        void set_validity(const AveragePrice & value) { this->validity = value; }

        const AveragePrice & get_validity_ttl() const { return validity_ttl; }
        AveragePrice & get_mutable_validity_ttl() { return validity_ttl; }
        void set_validity_ttl(const AveragePrice & value) { this->validity_ttl = value; }

        const AveragePrice & get_variety() const { return variety; }
        AveragePrice & get_mutable_variety() { return variety; }
        void set_variety(const AveragePrice & value) { this->variety = value; }
    };

    class Datum {
        public:
        Datum() = default;
        virtual ~Datum() = default;

        private:
        bool additional_properties;
        DatumProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const DatumProperties & get_properties() const { return properties; }
        DatumProperties & get_mutable_properties() { return properties; }
        void set_properties(const DatumProperties & value) { this->properties = value; }

        const std::vector<std::string> & get_required() const { return required; }
        std::vector<std::string> & get_mutable_required() { return required; }
        void set_required(const std::vector<std::string> & value) { this->required = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class IcebergProperties {
        public:
        IcebergProperties() = default;
        virtual ~IcebergProperties() = default;

        private:
        AveragePrice leg;
        AveragePrice leg_quantity;
        AveragePrice legs;
        AveragePrice remaining_quantity;
        AveragePrice total_quantity;

        public:
        const AveragePrice & get_leg() const { return leg; }
        AveragePrice & get_mutable_leg() { return leg; }
        void set_leg(const AveragePrice & value) { this->leg = value; }

        const AveragePrice & get_leg_quantity() const { return leg_quantity; }
        AveragePrice & get_mutable_leg_quantity() { return leg_quantity; }
        void set_leg_quantity(const AveragePrice & value) { this->leg_quantity = value; }

        const AveragePrice & get_legs() const { return legs; }
        AveragePrice & get_mutable_legs() { return legs; }
        void set_legs(const AveragePrice & value) { this->legs = value; }

        const AveragePrice & get_remaining_quantity() const { return remaining_quantity; }
        AveragePrice & get_mutable_remaining_quantity() { return remaining_quantity; }
        void set_remaining_quantity(const AveragePrice & value) { this->remaining_quantity = value; }

        const AveragePrice & get_total_quantity() const { return total_quantity; }
        AveragePrice & get_mutable_total_quantity() { return total_quantity; }
        void set_total_quantity(const AveragePrice & value) { this->total_quantity = value; }
    };

    class Iceberg {
        public:
        Iceberg() = default;
        virtual ~Iceberg() = default;

        private:
        bool additional_properties;
        IcebergProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const IcebergProperties & get_properties() const { return properties; }
        IcebergProperties & get_mutable_properties() { return properties; }
        void set_properties(const IcebergProperties & value) { this->properties = value; }

        const std::vector<std::string> & get_required() const { return required; }
        std::vector<std::string> & get_mutable_required() { return required; }
        void set_required(const std::vector<std::string> & value) { this->required = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class MetaProperties {
        public:
        MetaProperties() = default;
        virtual ~MetaProperties() = default;

        private:
        Meta iceberg;

        public:
        const Meta & get_iceberg() const { return iceberg; }
        Meta & get_mutable_iceberg() { return iceberg; }
        void set_iceberg(const Meta & value) { this->iceberg = value; }
    };

    class MetaClass {
        public:
        MetaClass() = default;
        virtual ~MetaClass() = default;

        private:
        bool additional_properties;
        MetaProperties properties;
        std::vector<nlohmann::json> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const MetaProperties & get_properties() const { return properties; }
        MetaProperties & get_mutable_properties() { return properties; }
        void set_properties(const MetaProperties & value) { this->properties = value; }

        const std::vector<nlohmann::json> & get_required() const { return required; }
        std::vector<nlohmann::json> & get_mutable_required() { return required; }
        void set_required(const std::vector<nlohmann::json> & value) { this->required = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        Meta items;
        std::string type;

        public:
        const Meta & get_items() const { return items; }
        Meta & get_mutable_items() { return items; }
        void set_items(const Meta & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class OrdersProperties {
        public:
        OrdersProperties() = default;
        virtual ~OrdersProperties() = default;

        private:
        Data data;
        AveragePrice status;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const AveragePrice & get_status() const { return status; }
        AveragePrice & get_mutable_status() { return status; }
        void set_status(const AveragePrice & value) { this->status = value; }
    };

    class OrdersClass {
        public:
        OrdersClass() = default;
        virtual ~OrdersClass() = default;

        private:
        bool additional_properties;
        OrdersProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const OrdersProperties & get_properties() const { return properties; }
        OrdersProperties & get_mutable_properties() { return properties; }
        void set_properties(const OrdersProperties & value) { this->properties = value; }

        const std::vector<std::string> & get_required() const { return required; }
        std::vector<std::string> & get_mutable_required() { return required; }
        void set_required(const std::vector<std::string> & value) { this->required = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class Definitions {
        public:
        Definitions() = default;
        virtual ~Definitions() = default;

        private:
        Datum datum;
        Iceberg iceberg;
        MetaClass meta;
        OrdersClass orders;

        public:
        const Datum & get_datum() const { return datum; }
        Datum & get_mutable_datum() { return datum; }
        void set_datum(const Datum & value) { this->datum = value; }

        const Iceberg & get_iceberg() const { return iceberg; }
        Iceberg & get_mutable_iceberg() { return iceberg; }
        void set_iceberg(const Iceberg & value) { this->iceberg = value; }

        const MetaClass & get_meta() const { return meta; }
        MetaClass & get_mutable_meta() { return meta; }
        void set_meta(const MetaClass & value) { this->meta = value; }

        const OrdersClass & get_orders() const { return orders; }
        OrdersClass & get_mutable_orders() { return orders; }
        void set_orders(const OrdersClass & value) { this->orders = value; }
    };

    class Orders {
        public:
        Orders() = default;
        virtual ~Orders() = default;

        private:
        std::string ref;
        std::string schema;
        Definitions definitions;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }

        const std::string & get_schema() const { return schema; }
        std::string & get_mutable_schema() { return schema; }
        void set_schema(const std::string & value) { this->schema = value; }

        const Definitions & get_definitions() const { return definitions; }
        Definitions & get_mutable_definitions() { return definitions; }
        void set_definitions(const Definitions & value) { this->definitions = value; }
    };
}

namespace nlohmann {
    void from_json(const json & j, quicktype::AveragePrice & x);
    void to_json(json & j, const quicktype::AveragePrice & x);

    void from_json(const json & j, quicktype::ExchangeOrderId & x);
    void to_json(json & j, const quicktype::ExchangeOrderId & x);

    void from_json(const json & j, quicktype::OrderTimestamp & x);
    void to_json(json & j, const quicktype::OrderTimestamp & x);

    void from_json(const json & j, quicktype::ExchangeETimestamp & x);
    void to_json(json & j, const quicktype::ExchangeETimestamp & x);

    void from_json(const json & j, quicktype::Meta & x);
    void to_json(json & j, const quicktype::Meta & x);

    void from_json(const json & j, quicktype::Tags & x);
    void to_json(json & j, const quicktype::Tags & x);

    void from_json(const json & j, quicktype::DatumProperties & x);
    void to_json(json & j, const quicktype::DatumProperties & x);

    void from_json(const json & j, quicktype::Datum & x);
    void to_json(json & j, const quicktype::Datum & x);

    void from_json(const json & j, quicktype::IcebergProperties & x);
    void to_json(json & j, const quicktype::IcebergProperties & x);

    void from_json(const json & j, quicktype::Iceberg & x);
    void to_json(json & j, const quicktype::Iceberg & x);

    void from_json(const json & j, quicktype::MetaProperties & x);
    void to_json(json & j, const quicktype::MetaProperties & x);

    void from_json(const json & j, quicktype::MetaClass & x);
    void to_json(json & j, const quicktype::MetaClass & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::OrdersProperties & x);
    void to_json(json & j, const quicktype::OrdersProperties & x);

    void from_json(const json & j, quicktype::OrdersClass & x);
    void to_json(json & j, const quicktype::OrdersClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::Orders & x);
    void to_json(json & j, const quicktype::Orders & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::AveragePrice& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::AveragePrice & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ExchangeOrderId& x) {
        x.set_any_of(j.at("anyOf").get<std::vector<quicktype::AveragePrice>>());
    }

    inline void to_json(json & j, const quicktype::ExchangeOrderId & x) {
        j = json::object();
        j["anyOf"] = x.get_any_of();
    }

    inline void from_json(const json & j, quicktype::OrderTimestamp& x) {
        x.set_format(quicktype::get_optional<std::string>(j, "format"));
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::OrderTimestamp & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ExchangeETimestamp& x) {
        x.set_any_of(j.at("anyOf").get<std::vector<quicktype::OrderTimestamp>>());
    }

    inline void to_json(json & j, const quicktype::ExchangeETimestamp & x) {
        j = json::object();
        j["anyOf"] = x.get_any_of();
    }

    inline void from_json(const json & j, quicktype::Meta& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Meta & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::Tags& x) {
        x.set_items(j.at("items").get<quicktype::AveragePrice>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Tags & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DatumProperties& x) {
        x.set_average_price(j.at("average_price").get<quicktype::AveragePrice>());
        x.set_cancelled_quantity(j.at("cancelled_quantity").get<quicktype::AveragePrice>());
        x.set_disclosed_quantity(j.at("disclosed_quantity").get<quicktype::AveragePrice>());
        x.set_exchange(j.at("exchange").get<quicktype::AveragePrice>());
        x.set_exchange_order_id(j.at("exchange_order_id").get<quicktype::ExchangeOrderId>());
        x.set_exchange_timestamp(j.at("exchange_timestamp").get<quicktype::ExchangeETimestamp>());
        x.set_exchange_update_timestamp(j.at("exchange_update_timestamp").get<quicktype::ExchangeETimestamp>());
        x.set_filled_quantity(j.at("filled_quantity").get<quicktype::AveragePrice>());
        x.set_guid(j.at("guid").get<quicktype::AveragePrice>());
        x.set_instrument_token(j.at("instrument_token").get<quicktype::AveragePrice>());
        x.set_market_protection(j.at("market_protection").get<quicktype::AveragePrice>());
        x.set_meta(j.at("meta").get<quicktype::Meta>());
        x.set_modified(j.at("modified").get<quicktype::AveragePrice>());
        x.set_order_id(j.at("order_id").get<quicktype::AveragePrice>());
        x.set_order_timestamp(j.at("order_timestamp").get<quicktype::OrderTimestamp>());
        x.set_order_type(j.at("order_type").get<quicktype::AveragePrice>());
        x.set_parent_order_id(j.at("parent_order_id").get<quicktype::AveragePrice>());
        x.set_pending_quantity(j.at("pending_quantity").get<quicktype::AveragePrice>());
        x.set_placed_by(j.at("placed_by").get<quicktype::AveragePrice>());
        x.set_price(j.at("price").get<quicktype::AveragePrice>());
        x.set_product(j.at("product").get<quicktype::AveragePrice>());
        x.set_quantity(j.at("quantity").get<quicktype::AveragePrice>());
        x.set_status(j.at("status").get<quicktype::AveragePrice>());
        x.set_status_message(j.at("status_message").get<quicktype::ExchangeOrderId>());
        x.set_status_message_raw(j.at("status_message_raw").get<quicktype::ExchangeOrderId>());
        x.set_tag(j.at("tag").get<quicktype::ExchangeOrderId>());
        x.set_tags(j.at("tags").get<quicktype::Tags>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::AveragePrice>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::AveragePrice>());
        x.set_trigger_price(j.at("trigger_price").get<quicktype::AveragePrice>());
        x.set_validity(j.at("validity").get<quicktype::AveragePrice>());
        x.set_validity_ttl(j.at("validity_ttl").get<quicktype::AveragePrice>());
        x.set_variety(j.at("variety").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::DatumProperties & x) {
        j = json::object();
        j["average_price"] = x.get_average_price();
        j["cancelled_quantity"] = x.get_cancelled_quantity();
        j["disclosed_quantity"] = x.get_disclosed_quantity();
        j["exchange"] = x.get_exchange();
        j["exchange_order_id"] = x.get_exchange_order_id();
        j["exchange_timestamp"] = x.get_exchange_timestamp();
        j["exchange_update_timestamp"] = x.get_exchange_update_timestamp();
        j["filled_quantity"] = x.get_filled_quantity();
        j["guid"] = x.get_guid();
        j["instrument_token"] = x.get_instrument_token();
        j["market_protection"] = x.get_market_protection();
        j["meta"] = x.get_meta();
        j["modified"] = x.get_modified();
        j["order_id"] = x.get_order_id();
        j["order_timestamp"] = x.get_order_timestamp();
        j["order_type"] = x.get_order_type();
        j["parent_order_id"] = x.get_parent_order_id();
        j["pending_quantity"] = x.get_pending_quantity();
        j["placed_by"] = x.get_placed_by();
        j["price"] = x.get_price();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["status"] = x.get_status();
        j["status_message"] = x.get_status_message();
        j["status_message_raw"] = x.get_status_message_raw();
        j["tag"] = x.get_tag();
        j["tags"] = x.get_tags();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["trigger_price"] = x.get_trigger_price();
        j["validity"] = x.get_validity();
        j["validity_ttl"] = x.get_validity_ttl();
        j["variety"] = x.get_variety();
    }

    inline void from_json(const json & j, quicktype::Datum& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::DatumProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Datum & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::IcebergProperties& x) {
        x.set_leg(j.at("leg").get<quicktype::AveragePrice>());
        x.set_leg_quantity(j.at("leg_quantity").get<quicktype::AveragePrice>());
        x.set_legs(j.at("legs").get<quicktype::AveragePrice>());
        x.set_remaining_quantity(j.at("remaining_quantity").get<quicktype::AveragePrice>());
        x.set_total_quantity(j.at("total_quantity").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::IcebergProperties & x) {
        j = json::object();
        j["leg"] = x.get_leg();
        j["leg_quantity"] = x.get_leg_quantity();
        j["legs"] = x.get_legs();
        j["remaining_quantity"] = x.get_remaining_quantity();
        j["total_quantity"] = x.get_total_quantity();
    }

    inline void from_json(const json & j, quicktype::Iceberg& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::IcebergProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Iceberg & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::MetaProperties& x) {
        x.set_iceberg(j.at("iceberg").get<quicktype::Meta>());
    }

    inline void to_json(json & j, const quicktype::MetaProperties & x) {
        j = json::object();
        j["iceberg"] = x.get_iceberg();
    }

    inline void from_json(const json & j, quicktype::MetaClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::MetaProperties>());
        x.set_required(j.at("required").get<std::vector<json>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MetaClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Data& x) {
        x.set_items(j.at("items").get<quicktype::Meta>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Data & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::OrdersProperties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
        x.set_status(j.at("status").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::OrdersProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::OrdersClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::OrdersProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::OrdersClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_datum(j.at("Datum").get<quicktype::Datum>());
        x.set_iceberg(j.at("Iceberg").get<quicktype::Iceberg>());
        x.set_meta(j.at("Meta").get<quicktype::MetaClass>());
        x.set_orders(j.at("Orders").get<quicktype::OrdersClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Datum"] = x.get_datum();
        j["Iceberg"] = x.get_iceberg();
        j["Meta"] = x.get_meta();
        j["Orders"] = x.get_orders();
    }

    inline void from_json(const json & j, quicktype::Orders& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::Orders & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }

    inline void from_json(const json & j, quicktype::Type & x) {
        if (j == "boolean") x = quicktype::Type::BOOLEAN;
        else if (j == "integer") x = quicktype::Type::INTEGER;
        else if (j == "string") x = quicktype::Type::STRING;
        else if (j == "null") x = quicktype::Type::TYPE_NULL;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const quicktype::Type & x) {
        switch (x) {
            case quicktype::Type::BOOLEAN: j = "boolean"; break;
            case quicktype::Type::INTEGER: j = "integer"; break;
            case quicktype::Type::STRING: j = "string"; break;
            case quicktype::Type::TYPE_NULL: j = "null"; break;
            default: throw "This should not happen";
        }
    }
}
