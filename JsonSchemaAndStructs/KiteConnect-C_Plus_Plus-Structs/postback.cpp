//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Postback data = nlohmann::json::parse(jsonString);

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

namespace Postback {
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

    class Meta {
        public:
        Meta() = default;
        virtual ~Meta() = default;

        private:

        public:
    };

    class Postback {
        public:
        Postback() = default;
        virtual ~Postback() = default;

        private:
        std::shared_ptr<int64_t> app_id;
        std::shared_ptr<int64_t> average_price;
        std::shared_ptr<int64_t> cancelled_quantity;
        std::shared_ptr<std::string> checksum;
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
        nlohmann::json status_message;
        nlohmann::json status_message_raw;
        nlohmann::json tag;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> transaction_type;
        std::shared_ptr<int64_t> trigger_price;
        std::shared_ptr<int64_t> unfilled_quantity;
        std::shared_ptr<std::string> user_id;
        std::shared_ptr<std::string> validity;
        std::shared_ptr<std::string> variety;

        public:
        std::shared_ptr<int64_t> get_app_id() const { return app_id; }
        void set_app_id(std::shared_ptr<int64_t> value) { this->app_id = value; }

        std::shared_ptr<int64_t> get_average_price() const { return average_price; }
        void set_average_price(std::shared_ptr<int64_t> value) { this->average_price = value; }

        std::shared_ptr<int64_t> get_cancelled_quantity() const { return cancelled_quantity; }
        void set_cancelled_quantity(std::shared_ptr<int64_t> value) { this->cancelled_quantity = value; }

        std::shared_ptr<std::string> get_checksum() const { return checksum; }
        void set_checksum(std::shared_ptr<std::string> value) { this->checksum = value; }

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

        const nlohmann::json & get_status_message() const { return status_message; }
        nlohmann::json & get_mutable_status_message() { return status_message; }
        void set_status_message(const nlohmann::json & value) { this->status_message = value; }

        const nlohmann::json & get_status_message_raw() const { return status_message_raw; }
        nlohmann::json & get_mutable_status_message_raw() { return status_message_raw; }
        void set_status_message_raw(const nlohmann::json & value) { this->status_message_raw = value; }

        const nlohmann::json & get_tag() const { return tag; }
        nlohmann::json & get_mutable_tag() { return tag; }
        void set_tag(const nlohmann::json & value) { this->tag = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_transaction_type() const { return transaction_type; }
        void set_transaction_type(std::shared_ptr<std::string> value) { this->transaction_type = value; }

        std::shared_ptr<int64_t> get_trigger_price() const { return trigger_price; }
        void set_trigger_price(std::shared_ptr<int64_t> value) { this->trigger_price = value; }

        std::shared_ptr<int64_t> get_unfilled_quantity() const { return unfilled_quantity; }
        void set_unfilled_quantity(std::shared_ptr<int64_t> value) { this->unfilled_quantity = value; }

        std::shared_ptr<std::string> get_user_id() const { return user_id; }
        void set_user_id(std::shared_ptr<std::string> value) { this->user_id = value; }

        std::shared_ptr<std::string> get_validity() const { return validity; }
        void set_validity(std::shared_ptr<std::string> value) { this->validity = value; }

        std::shared_ptr<std::string> get_variety() const { return variety; }
        void set_variety(std::shared_ptr<std::string> value) { this->variety = value; }
    };
}

namespace nlohmann {
    void from_json(const json & j, Postback::Meta & x);
    void to_json(json & j, const Postback::Meta & x);

    void from_json(const json & j, Postback::Postback & x);
    void to_json(json & j, const Postback::Postback & x);

    inline void from_json(const json & j, Postback::Meta& x) {
    }

    inline void to_json(json & j, const Postback::Meta & x) {
        j = json::object();
    }

    inline void from_json(const json & j, Postback::Postback& x) {
        x.set_app_id(Postback::get_optional<int64_t>(j, "app_id"));
        x.set_average_price(Postback::get_optional<int64_t>(j, "average_price"));
        x.set_cancelled_quantity(Postback::get_optional<int64_t>(j, "cancelled_quantity"));
        x.set_checksum(Postback::get_optional<std::string>(j, "checksum"));
        x.set_disclosed_quantity(Postback::get_optional<int64_t>(j, "disclosed_quantity"));
        x.set_exchange(Postback::get_optional<std::string>(j, "exchange"));
        x.set_exchange_order_id(Postback::get_optional<std::string>(j, "exchange_order_id"));
        x.set_exchange_timestamp(Postback::get_optional<std::string>(j, "exchange_timestamp"));
        x.set_exchange_update_timestamp(Postback::get_optional<std::string>(j, "exchange_update_timestamp"));
        x.set_filled_quantity(Postback::get_optional<int64_t>(j, "filled_quantity"));
        x.set_guid(Postback::get_optional<std::string>(j, "guid"));
        x.set_instrument_token(Postback::get_optional<int64_t>(j, "instrument_token"));
        x.set_market_protection(Postback::get_optional<int64_t>(j, "market_protection"));
        x.set_meta(Postback::get_optional<Postback::Meta>(j, "meta"));
        x.set_order_id(Postback::get_optional<std::string>(j, "order_id"));
        x.set_order_timestamp(Postback::get_optional<std::string>(j, "order_timestamp"));
        x.set_order_type(Postback::get_optional<std::string>(j, "order_type"));
        x.set_parent_order_id(Postback::get_untyped(j, "parent_order_id"));
        x.set_pending_quantity(Postback::get_optional<int64_t>(j, "pending_quantity"));
        x.set_placed_by(Postback::get_optional<std::string>(j, "placed_by"));
        x.set_price(Postback::get_optional<int64_t>(j, "price"));
        x.set_product(Postback::get_optional<std::string>(j, "product"));
        x.set_quantity(Postback::get_optional<int64_t>(j, "quantity"));
        x.set_status(Postback::get_optional<std::string>(j, "status"));
        x.set_status_message(Postback::get_untyped(j, "status_message"));
        x.set_status_message_raw(Postback::get_untyped(j, "status_message_raw"));
        x.set_tag(Postback::get_untyped(j, "tag"));
        x.set_tradingsymbol(Postback::get_optional<std::string>(j, "tradingsymbol"));
        x.set_transaction_type(Postback::get_optional<std::string>(j, "transaction_type"));
        x.set_trigger_price(Postback::get_optional<int64_t>(j, "trigger_price"));
        x.set_unfilled_quantity(Postback::get_optional<int64_t>(j, "unfilled_quantity"));
        x.set_user_id(Postback::get_optional<std::string>(j, "user_id"));
        x.set_validity(Postback::get_optional<std::string>(j, "validity"));
        x.set_variety(Postback::get_optional<std::string>(j, "variety"));
    }

    inline void to_json(json & j, const Postback::Postback & x) {
        j = json::object();
        j["app_id"] = x.get_app_id();
        j["average_price"] = x.get_average_price();
        j["cancelled_quantity"] = x.get_cancelled_quantity();
        j["checksum"] = x.get_checksum();
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
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["trigger_price"] = x.get_trigger_price();
        j["unfilled_quantity"] = x.get_unfilled_quantity();
        j["user_id"] = x.get_user_id();
        j["validity"] = x.get_validity();
        j["variety"] = x.get_variety();
    }
}
