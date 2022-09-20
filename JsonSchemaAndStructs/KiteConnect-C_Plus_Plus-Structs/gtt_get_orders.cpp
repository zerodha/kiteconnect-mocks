//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     GttGetOrders data = nlohmann::json::parse(jsonString);

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

namespace GttGetOrders {
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

    class Condition {
        public:
        Condition() = default;
        virtual ~Condition() = default;

        private:
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<double> last_price;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::vector<double>> trigger_values;

        public:
        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<double> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<double> value) { this->last_price = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::vector<double>> get_trigger_values() const { return trigger_values; }
        void set_trigger_values(std::shared_ptr<std::vector<double>> value) { this->trigger_values = value; }
    };

    class Meta {
        public:
        Meta() = default;
        virtual ~Meta() = default;

        private:

        public:
    };

    class OrderResult {
        public:
        OrderResult() = default;
        virtual ~OrderResult() = default;

        private:
        std::shared_ptr<std::string> order_id;
        std::shared_ptr<std::string> rejection_reason;
        std::shared_ptr<std::string> status;

        public:
        std::shared_ptr<std::string> get_order_id() const { return order_id; }
        void set_order_id(std::shared_ptr<std::string> value) { this->order_id = value; }

        std::shared_ptr<std::string> get_rejection_reason() const { return rejection_reason; }
        void set_rejection_reason(std::shared_ptr<std::string> value) { this->rejection_reason = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }
    };

    class Result {
        public:
        Result() = default;
        virtual ~Result() = default;

        private:
        std::shared_ptr<std::string> account_id;
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<std::string> meta;
        std::shared_ptr<OrderResult> order_result;
        std::shared_ptr<std::string> order_type;
        std::shared_ptr<int64_t> price;
        std::shared_ptr<std::string> product;
        std::shared_ptr<int64_t> quantity;
        std::shared_ptr<std::string> timestamp;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> transaction_type;
        std::shared_ptr<double> triggered_at;
        std::shared_ptr<std::string> validity;

        public:
        std::shared_ptr<std::string> get_account_id() const { return account_id; }
        void set_account_id(std::shared_ptr<std::string> value) { this->account_id = value; }

        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<std::string> get_meta() const { return meta; }
        void set_meta(std::shared_ptr<std::string> value) { this->meta = value; }

        std::shared_ptr<OrderResult> get_order_result() const { return order_result; }
        void set_order_result(std::shared_ptr<OrderResult> value) { this->order_result = value; }

        std::shared_ptr<std::string> get_order_type() const { return order_type; }
        void set_order_type(std::shared_ptr<std::string> value) { this->order_type = value; }

        std::shared_ptr<int64_t> get_price() const { return price; }
        void set_price(std::shared_ptr<int64_t> value) { this->price = value; }

        std::shared_ptr<std::string> get_product() const { return product; }
        void set_product(std::shared_ptr<std::string> value) { this->product = value; }

        std::shared_ptr<int64_t> get_quantity() const { return quantity; }
        void set_quantity(std::shared_ptr<int64_t> value) { this->quantity = value; }

        std::shared_ptr<std::string> get_timestamp() const { return timestamp; }
        void set_timestamp(std::shared_ptr<std::string> value) { this->timestamp = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_transaction_type() const { return transaction_type; }
        void set_transaction_type(std::shared_ptr<std::string> value) { this->transaction_type = value; }

        std::shared_ptr<double> get_triggered_at() const { return triggered_at; }
        void set_triggered_at(std::shared_ptr<double> value) { this->triggered_at = value; }

        std::shared_ptr<std::string> get_validity() const { return validity; }
        void set_validity(std::shared_ptr<std::string> value) { this->validity = value; }
    };

    class Order {
        public:
        Order() = default;
        virtual ~Order() = default;

        private:
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<std::string> order_type;
        std::shared_ptr<double> price;
        std::shared_ptr<std::string> product;
        std::shared_ptr<int64_t> quantity;
        std::shared_ptr<Result> result;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> transaction_type;

        public:
        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<std::string> get_order_type() const { return order_type; }
        void set_order_type(std::shared_ptr<std::string> value) { this->order_type = value; }

        std::shared_ptr<double> get_price() const { return price; }
        void set_price(std::shared_ptr<double> value) { this->price = value; }

        std::shared_ptr<std::string> get_product() const { return product; }
        void set_product(std::shared_ptr<std::string> value) { this->product = value; }

        std::shared_ptr<int64_t> get_quantity() const { return quantity; }
        void set_quantity(std::shared_ptr<int64_t> value) { this->quantity = value; }

        std::shared_ptr<Result> get_result() const { return result; }
        void set_result(std::shared_ptr<Result> value) { this->result = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_transaction_type() const { return transaction_type; }
        void set_transaction_type(std::shared_ptr<std::string> value) { this->transaction_type = value; }
    };

    class Datum {
        public:
        Datum() = default;
        virtual ~Datum() = default;

        private:
        std::shared_ptr<Condition> condition;
        std::shared_ptr<std::string> created_at;
        std::shared_ptr<std::string> expires_at;
        std::shared_ptr<int64_t> id;
        std::shared_ptr<Meta> meta;
        std::shared_ptr<std::vector<Order>> orders;
        nlohmann::json parent_trigger;
        std::shared_ptr<std::string> status;
        std::shared_ptr<std::string> type;
        std::shared_ptr<std::string> updated_at;
        std::shared_ptr<std::string> user_id;

        public:
        std::shared_ptr<Condition> get_condition() const { return condition; }
        void set_condition(std::shared_ptr<Condition> value) { this->condition = value; }

        std::shared_ptr<std::string> get_created_at() const { return created_at; }
        void set_created_at(std::shared_ptr<std::string> value) { this->created_at = value; }

        std::shared_ptr<std::string> get_expires_at() const { return expires_at; }
        void set_expires_at(std::shared_ptr<std::string> value) { this->expires_at = value; }

        std::shared_ptr<int64_t> get_id() const { return id; }
        void set_id(std::shared_ptr<int64_t> value) { this->id = value; }

        std::shared_ptr<Meta> get_meta() const { return meta; }
        void set_meta(std::shared_ptr<Meta> value) { this->meta = value; }

        std::shared_ptr<std::vector<Order>> get_orders() const { return orders; }
        void set_orders(std::shared_ptr<std::vector<Order>> value) { this->orders = value; }

        const nlohmann::json & get_parent_trigger() const { return parent_trigger; }
        nlohmann::json & get_mutable_parent_trigger() { return parent_trigger; }
        void set_parent_trigger(const nlohmann::json & value) { this->parent_trigger = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }

        std::shared_ptr<std::string> get_type() const { return type; }
        void set_type(std::shared_ptr<std::string> value) { this->type = value; }

        std::shared_ptr<std::string> get_updated_at() const { return updated_at; }
        void set_updated_at(std::shared_ptr<std::string> value) { this->updated_at = value; }

        std::shared_ptr<std::string> get_user_id() const { return user_id; }
        void set_user_id(std::shared_ptr<std::string> value) { this->user_id = value; }
    };

    class GttGetOrders {
        public:
        GttGetOrders() = default;
        virtual ~GttGetOrders() = default;

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
    void from_json(const json & j, GttGetOrders::Condition & x);
    void to_json(json & j, const GttGetOrders::Condition & x);

    void from_json(const json & j, GttGetOrders::Meta & x);
    void to_json(json & j, const GttGetOrders::Meta & x);

    void from_json(const json & j, GttGetOrders::OrderResult & x);
    void to_json(json & j, const GttGetOrders::OrderResult & x);

    void from_json(const json & j, GttGetOrders::Result & x);
    void to_json(json & j, const GttGetOrders::Result & x);

    void from_json(const json & j, GttGetOrders::Order & x);
    void to_json(json & j, const GttGetOrders::Order & x);

    void from_json(const json & j, GttGetOrders::Datum & x);
    void to_json(json & j, const GttGetOrders::Datum & x);

    void from_json(const json & j, GttGetOrders::GttGetOrders & x);
    void to_json(json & j, const GttGetOrders::GttGetOrders & x);

    inline void from_json(const json & j, GttGetOrders::Condition& x) {
        x.set_exchange(GttGetOrders::get_optional<std::string>(j, "exchange"));
        x.set_instrument_token(GttGetOrders::get_optional<int64_t>(j, "instrument_token"));
        x.set_last_price(GttGetOrders::get_optional<double>(j, "last_price"));
        x.set_tradingsymbol(GttGetOrders::get_optional<std::string>(j, "tradingsymbol"));
        x.set_trigger_values(GttGetOrders::get_optional<std::vector<double>>(j, "trigger_values"));
    }

    inline void to_json(json & j, const GttGetOrders::Condition & x) {
        j = json::object();
        j["exchange"] = x.get_exchange();
        j["instrument_token"] = x.get_instrument_token();
        j["last_price"] = x.get_last_price();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["trigger_values"] = x.get_trigger_values();
    }

    inline void from_json(const json & j, GttGetOrders::Meta& x) {
    }

    inline void to_json(json & j, const GttGetOrders::Meta & x) {
        j = json::object();
    }

    inline void from_json(const json & j, GttGetOrders::OrderResult& x) {
        x.set_order_id(GttGetOrders::get_optional<std::string>(j, "order_id"));
        x.set_rejection_reason(GttGetOrders::get_optional<std::string>(j, "rejection_reason"));
        x.set_status(GttGetOrders::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const GttGetOrders::OrderResult & x) {
        j = json::object();
        j["order_id"] = x.get_order_id();
        j["rejection_reason"] = x.get_rejection_reason();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, GttGetOrders::Result& x) {
        x.set_account_id(GttGetOrders::get_optional<std::string>(j, "account_id"));
        x.set_exchange(GttGetOrders::get_optional<std::string>(j, "exchange"));
        x.set_meta(GttGetOrders::get_optional<std::string>(j, "meta"));
        x.set_order_result(GttGetOrders::get_optional<GttGetOrders::OrderResult>(j, "order_result"));
        x.set_order_type(GttGetOrders::get_optional<std::string>(j, "order_type"));
        x.set_price(GttGetOrders::get_optional<int64_t>(j, "price"));
        x.set_product(GttGetOrders::get_optional<std::string>(j, "product"));
        x.set_quantity(GttGetOrders::get_optional<int64_t>(j, "quantity"));
        x.set_timestamp(GttGetOrders::get_optional<std::string>(j, "timestamp"));
        x.set_tradingsymbol(GttGetOrders::get_optional<std::string>(j, "tradingsymbol"));
        x.set_transaction_type(GttGetOrders::get_optional<std::string>(j, "transaction_type"));
        x.set_triggered_at(GttGetOrders::get_optional<double>(j, "triggered_at"));
        x.set_validity(GttGetOrders::get_optional<std::string>(j, "validity"));
    }

    inline void to_json(json & j, const GttGetOrders::Result & x) {
        j = json::object();
        j["account_id"] = x.get_account_id();
        j["exchange"] = x.get_exchange();
        j["meta"] = x.get_meta();
        j["order_result"] = x.get_order_result();
        j["order_type"] = x.get_order_type();
        j["price"] = x.get_price();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["timestamp"] = x.get_timestamp();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["triggered_at"] = x.get_triggered_at();
        j["validity"] = x.get_validity();
    }

    inline void from_json(const json & j, GttGetOrders::Order& x) {
        x.set_exchange(GttGetOrders::get_optional<std::string>(j, "exchange"));
        x.set_order_type(GttGetOrders::get_optional<std::string>(j, "order_type"));
        x.set_price(GttGetOrders::get_optional<double>(j, "price"));
        x.set_product(GttGetOrders::get_optional<std::string>(j, "product"));
        x.set_quantity(GttGetOrders::get_optional<int64_t>(j, "quantity"));
        x.set_result(GttGetOrders::get_optional<GttGetOrders::Result>(j, "result"));
        x.set_tradingsymbol(GttGetOrders::get_optional<std::string>(j, "tradingsymbol"));
        x.set_transaction_type(GttGetOrders::get_optional<std::string>(j, "transaction_type"));
    }

    inline void to_json(json & j, const GttGetOrders::Order & x) {
        j = json::object();
        j["exchange"] = x.get_exchange();
        j["order_type"] = x.get_order_type();
        j["price"] = x.get_price();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["result"] = x.get_result();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
    }

    inline void from_json(const json & j, GttGetOrders::Datum& x) {
        x.set_condition(GttGetOrders::get_optional<GttGetOrders::Condition>(j, "condition"));
        x.set_created_at(GttGetOrders::get_optional<std::string>(j, "created_at"));
        x.set_expires_at(GttGetOrders::get_optional<std::string>(j, "expires_at"));
        x.set_id(GttGetOrders::get_optional<int64_t>(j, "id"));
        x.set_meta(GttGetOrders::get_optional<GttGetOrders::Meta>(j, "meta"));
        x.set_orders(GttGetOrders::get_optional<std::vector<GttGetOrders::Order>>(j, "orders"));
        x.set_parent_trigger(GttGetOrders::get_untyped(j, "parent_trigger"));
        x.set_status(GttGetOrders::get_optional<std::string>(j, "status"));
        x.set_type(GttGetOrders::get_optional<std::string>(j, "type"));
        x.set_updated_at(GttGetOrders::get_optional<std::string>(j, "updated_at"));
        x.set_user_id(GttGetOrders::get_optional<std::string>(j, "user_id"));
    }

    inline void to_json(json & j, const GttGetOrders::Datum & x) {
        j = json::object();
        j["condition"] = x.get_condition();
        j["created_at"] = x.get_created_at();
        j["expires_at"] = x.get_expires_at();
        j["id"] = x.get_id();
        j["meta"] = x.get_meta();
        j["orders"] = x.get_orders();
        j["parent_trigger"] = x.get_parent_trigger();
        j["status"] = x.get_status();
        j["type"] = x.get_type();
        j["updated_at"] = x.get_updated_at();
        j["user_id"] = x.get_user_id();
    }

    inline void from_json(const json & j, GttGetOrders::GttGetOrders& x) {
        x.set_data(GttGetOrders::get_optional<std::vector<GttGetOrders::Datum>>(j, "data"));
        x.set_status(GttGetOrders::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const GttGetOrders::GttGetOrders & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
