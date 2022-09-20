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

namespace Orders {
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

    class Iceberg {
        public:
        Iceberg() = default;
        virtual ~Iceberg() = default;

        private:
        std::shared_ptr<int64_t> leg;
        std::shared_ptr<int64_t> leg_quantity;
        std::shared_ptr<int64_t> legs;
        std::shared_ptr<int64_t> remaining_quantity;
        std::shared_ptr<int64_t> total_quantity;

        public:
        std::shared_ptr<int64_t> get_leg() const { return leg; }
        void set_leg(std::shared_ptr<int64_t> value) { this->leg = value; }

        std::shared_ptr<int64_t> get_leg_quantity() const { return leg_quantity; }
        void set_leg_quantity(std::shared_ptr<int64_t> value) { this->leg_quantity = value; }

        std::shared_ptr<int64_t> get_legs() const { return legs; }
        void set_legs(std::shared_ptr<int64_t> value) { this->legs = value; }

        std::shared_ptr<int64_t> get_remaining_quantity() const { return remaining_quantity; }
        void set_remaining_quantity(std::shared_ptr<int64_t> value) { this->remaining_quantity = value; }

        std::shared_ptr<int64_t> get_total_quantity() const { return total_quantity; }
        void set_total_quantity(std::shared_ptr<int64_t> value) { this->total_quantity = value; }
    };

    class Meta {
        public:
        Meta() = default;
        virtual ~Meta() = default;

        private:
        std::shared_ptr<Iceberg> iceberg;

        public:
        std::shared_ptr<Iceberg> get_iceberg() const { return iceberg; }
        void set_iceberg(std::shared_ptr<Iceberg> value) { this->iceberg = value; }
    };

    class Datum {
        public:
        Datum() = default;
        virtual ~Datum() = default;

        private:
        std::shared_ptr<int64_t> average_price;
        std::shared_ptr<int64_t> cancelled_quantity;
        std::shared_ptr<int64_t> disclosed_quantity;
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<std::string> exchange_order_id;
        std::shared_ptr<std::string> exchange_timestamp;
        std::shared_ptr<std::string> exchange_update_timestamp;
        std::shared_ptr<int64_t> filled_quantity;
        std::shared_ptr<std::string> guid;
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<int64_t> market_protection;
        std::shared_ptr<Meta> meta;
        std::shared_ptr<bool> modified;
        std::shared_ptr<std::string> order_id;
        std::shared_ptr<std::string> order_timestamp;
        std::shared_ptr<std::string> order_type;
        nlohmann::json parent_order_id;
        std::shared_ptr<int64_t> pending_quantity;
        std::shared_ptr<std::string> placed_by;
        std::shared_ptr<int64_t> price;
        std::shared_ptr<std::string> product;
        std::shared_ptr<int64_t> quantity;
        std::shared_ptr<std::string> status;
        std::shared_ptr<std::string> status_message;
        std::shared_ptr<std::string> status_message_raw;
        std::shared_ptr<std::string> tag;
        std::shared_ptr<std::vector<std::string>> tags;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> transaction_type;
        std::shared_ptr<int64_t> trigger_price;
        std::shared_ptr<std::string> validity;
        std::shared_ptr<int64_t> validity_ttl;
        std::shared_ptr<std::string> variety;

        public:
        std::shared_ptr<int64_t> get_average_price() const { return average_price; }
        void set_average_price(std::shared_ptr<int64_t> value) { this->average_price = value; }

        std::shared_ptr<int64_t> get_cancelled_quantity() const { return cancelled_quantity; }
        void set_cancelled_quantity(std::shared_ptr<int64_t> value) { this->cancelled_quantity = value; }

        std::shared_ptr<int64_t> get_disclosed_quantity() const { return disclosed_quantity; }
        void set_disclosed_quantity(std::shared_ptr<int64_t> value) { this->disclosed_quantity = value; }

        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<std::string> get_exchange_order_id() const { return exchange_order_id; }
        void set_exchange_order_id(std::shared_ptr<std::string> value) { this->exchange_order_id = value; }

        std::shared_ptr<std::string> get_exchange_timestamp() const { return exchange_timestamp; }
        void set_exchange_timestamp(std::shared_ptr<std::string> value) { this->exchange_timestamp = value; }

        std::shared_ptr<std::string> get_exchange_update_timestamp() const { return exchange_update_timestamp; }
        void set_exchange_update_timestamp(std::shared_ptr<std::string> value) { this->exchange_update_timestamp = value; }

        std::shared_ptr<int64_t> get_filled_quantity() const { return filled_quantity; }
        void set_filled_quantity(std::shared_ptr<int64_t> value) { this->filled_quantity = value; }

        std::shared_ptr<std::string> get_guid() const { return guid; }
        void set_guid(std::shared_ptr<std::string> value) { this->guid = value; }

        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<int64_t> get_market_protection() const { return market_protection; }
        void set_market_protection(std::shared_ptr<int64_t> value) { this->market_protection = value; }

        std::shared_ptr<Meta> get_meta() const { return meta; }
        void set_meta(std::shared_ptr<Meta> value) { this->meta = value; }

        std::shared_ptr<bool> get_modified() const { return modified; }
        void set_modified(std::shared_ptr<bool> value) { this->modified = value; }

        std::shared_ptr<std::string> get_order_id() const { return order_id; }
        void set_order_id(std::shared_ptr<std::string> value) { this->order_id = value; }

        std::shared_ptr<std::string> get_order_timestamp() const { return order_timestamp; }
        void set_order_timestamp(std::shared_ptr<std::string> value) { this->order_timestamp = value; }

        std::shared_ptr<std::string> get_order_type() const { return order_type; }
        void set_order_type(std::shared_ptr<std::string> value) { this->order_type = value; }

        const nlohmann::json & get_parent_order_id() const { return parent_order_id; }
        nlohmann::json & get_mutable_parent_order_id() { return parent_order_id; }
        void set_parent_order_id(const nlohmann::json & value) { this->parent_order_id = value; }

        std::shared_ptr<int64_t> get_pending_quantity() const { return pending_quantity; }
        void set_pending_quantity(std::shared_ptr<int64_t> value) { this->pending_quantity = value; }

        std::shared_ptr<std::string> get_placed_by() const { return placed_by; }
        void set_placed_by(std::shared_ptr<std::string> value) { this->placed_by = value; }

        std::shared_ptr<int64_t> get_price() const { return price; }
        void set_price(std::shared_ptr<int64_t> value) { this->price = value; }

        std::shared_ptr<std::string> get_product() const { return product; }
        void set_product(std::shared_ptr<std::string> value) { this->product = value; }

        std::shared_ptr<int64_t> get_quantity() const { return quantity; }
        void set_quantity(std::shared_ptr<int64_t> value) { this->quantity = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }

        std::shared_ptr<std::string> get_status_message() const { return status_message; }
        void set_status_message(std::shared_ptr<std::string> value) { this->status_message = value; }

        std::shared_ptr<std::string> get_status_message_raw() const { return status_message_raw; }
        void set_status_message_raw(std::shared_ptr<std::string> value) { this->status_message_raw = value; }

        std::shared_ptr<std::string> get_tag() const { return tag; }
        void set_tag(std::shared_ptr<std::string> value) { this->tag = value; }

        std::shared_ptr<std::vector<std::string>> get_tags() const { return tags; }
        void set_tags(std::shared_ptr<std::vector<std::string>> value) { this->tags = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_transaction_type() const { return transaction_type; }
        void set_transaction_type(std::shared_ptr<std::string> value) { this->transaction_type = value; }

        std::shared_ptr<int64_t> get_trigger_price() const { return trigger_price; }
        void set_trigger_price(std::shared_ptr<int64_t> value) { this->trigger_price = value; }

        std::shared_ptr<std::string> get_validity() const { return validity; }
        void set_validity(std::shared_ptr<std::string> value) { this->validity = value; }

        std::shared_ptr<int64_t> get_validity_ttl() const { return validity_ttl; }
        void set_validity_ttl(std::shared_ptr<int64_t> value) { this->validity_ttl = value; }

        std::shared_ptr<std::string> get_variety() const { return variety; }
        void set_variety(std::shared_ptr<std::string> value) { this->variety = value; }
    };

    class Orders {
        public:
        Orders() = default;
        virtual ~Orders() = default;

        private:
        std::shared_ptr<std::vector<Datum>> data;
        std::shared_ptr<std::string> status;

        public:
        std::shared_ptr<std::vector<Datum>> get_data() const { return data; }
        void set_data(std::shared_ptr<std::vector<Datum>> value) { this->data = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }
    };
}

namespace nlohmann {
    void from_json(const json & j, Orders::Iceberg & x);
    void to_json(json & j, const Orders::Iceberg & x);

    void from_json(const json & j, Orders::Meta & x);
    void to_json(json & j, const Orders::Meta & x);

    void from_json(const json & j, Orders::Datum & x);
    void to_json(json & j, const Orders::Datum & x);

    void from_json(const json & j, Orders::Orders & x);
    void to_json(json & j, const Orders::Orders & x);

    inline void from_json(const json & j, Orders::Iceberg& x) {
        x.set_leg(Orders::get_optional<int64_t>(j, "leg"));
        x.set_leg_quantity(Orders::get_optional<int64_t>(j, "leg_quantity"));
        x.set_legs(Orders::get_optional<int64_t>(j, "legs"));
        x.set_remaining_quantity(Orders::get_optional<int64_t>(j, "remaining_quantity"));
        x.set_total_quantity(Orders::get_optional<int64_t>(j, "total_quantity"));
    }

    inline void to_json(json & j, const Orders::Iceberg & x) {
        j = json::object();
        j["leg"] = x.get_leg();
        j["leg_quantity"] = x.get_leg_quantity();
        j["legs"] = x.get_legs();
        j["remaining_quantity"] = x.get_remaining_quantity();
        j["total_quantity"] = x.get_total_quantity();
    }

    inline void from_json(const json & j, Orders::Meta& x) {
        x.set_iceberg(Orders::get_optional<Orders::Iceberg>(j, "iceberg"));
    }

    inline void to_json(json & j, const Orders::Meta & x) {
        j = json::object();
        j["iceberg"] = x.get_iceberg();
    }

    inline void from_json(const json & j, Orders::Datum& x) {
        x.set_average_price(Orders::get_optional<int64_t>(j, "average_price"));
        x.set_cancelled_quantity(Orders::get_optional<int64_t>(j, "cancelled_quantity"));
        x.set_disclosed_quantity(Orders::get_optional<int64_t>(j, "disclosed_quantity"));
        x.set_exchange(Orders::get_optional<std::string>(j, "exchange"));
        x.set_exchange_order_id(Orders::get_optional<std::string>(j, "exchange_order_id"));
        x.set_exchange_timestamp(Orders::get_optional<std::string>(j, "exchange_timestamp"));
        x.set_exchange_update_timestamp(Orders::get_optional<std::string>(j, "exchange_update_timestamp"));
        x.set_filled_quantity(Orders::get_optional<int64_t>(j, "filled_quantity"));
        x.set_guid(Orders::get_optional<std::string>(j, "guid"));
        x.set_instrument_token(Orders::get_optional<int64_t>(j, "instrument_token"));
        x.set_market_protection(Orders::get_optional<int64_t>(j, "market_protection"));
        x.set_meta(Orders::get_optional<Orders::Meta>(j, "meta"));
        x.set_modified(Orders::get_optional<bool>(j, "modified"));
        x.set_order_id(Orders::get_optional<std::string>(j, "order_id"));
        x.set_order_timestamp(Orders::get_optional<std::string>(j, "order_timestamp"));
        x.set_order_type(Orders::get_optional<std::string>(j, "order_type"));
        x.set_parent_order_id(Orders::get_untyped(j, "parent_order_id"));
        x.set_pending_quantity(Orders::get_optional<int64_t>(j, "pending_quantity"));
        x.set_placed_by(Orders::get_optional<std::string>(j, "placed_by"));
        x.set_price(Orders::get_optional<int64_t>(j, "price"));
        x.set_product(Orders::get_optional<std::string>(j, "product"));
        x.set_quantity(Orders::get_optional<int64_t>(j, "quantity"));
        x.set_status(Orders::get_optional<std::string>(j, "status"));
        x.set_status_message(Orders::get_optional<std::string>(j, "status_message"));
        x.set_status_message_raw(Orders::get_optional<std::string>(j, "status_message_raw"));
        x.set_tag(Orders::get_optional<std::string>(j, "tag"));
        x.set_tags(Orders::get_optional<std::vector<std::string>>(j, "tags"));
        x.set_tradingsymbol(Orders::get_optional<std::string>(j, "tradingsymbol"));
        x.set_transaction_type(Orders::get_optional<std::string>(j, "transaction_type"));
        x.set_trigger_price(Orders::get_optional<int64_t>(j, "trigger_price"));
        x.set_validity(Orders::get_optional<std::string>(j, "validity"));
        x.set_validity_ttl(Orders::get_optional<int64_t>(j, "validity_ttl"));
        x.set_variety(Orders::get_optional<std::string>(j, "variety"));
    }

    inline void to_json(json & j, const Orders::Datum & x) {
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

    inline void from_json(const json & j, Orders::Orders& x) {
        x.set_data(Orders::get_optional<std::vector<Orders::Datum>>(j, "data"));
        x.set_status(Orders::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const Orders::Orders & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
