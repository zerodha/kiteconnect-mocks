//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Trades data = nlohmann::json::parse(jsonString);

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

namespace  Trades {
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

    class Datum {
        public:
        Datum() = default;
        virtual ~Datum() = default;

        private:
        std::shared_ptr<double> average_price;
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<std::string> exchange_order_id;
        std::shared_ptr<std::string> exchange_timestamp;
        std::shared_ptr<std::string> fill_timestamp;
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<std::string> order_id;
        std::shared_ptr<std::string> order_timestamp;
        std::shared_ptr<std::string> product;
        std::shared_ptr<int64_t> quantity;
        std::shared_ptr<std::string> trade_id;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> transaction_type;

        public:
        std::shared_ptr<double> get_average_price() const { return average_price; }
        void set_average_price(std::shared_ptr<double> value) { this->average_price = value; }

        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<std::string> get_exchange_order_id() const { return exchange_order_id; }
        void set_exchange_order_id(std::shared_ptr<std::string> value) { this->exchange_order_id = value; }

        std::shared_ptr<std::string> get_exchange_timestamp() const { return exchange_timestamp; }
        void set_exchange_timestamp(std::shared_ptr<std::string> value) { this->exchange_timestamp = value; }

        std::shared_ptr<std::string> get_fill_timestamp() const { return fill_timestamp; }
        void set_fill_timestamp(std::shared_ptr<std::string> value) { this->fill_timestamp = value; }

        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<std::string> get_order_id() const { return order_id; }
        void set_order_id(std::shared_ptr<std::string> value) { this->order_id = value; }

        std::shared_ptr<std::string> get_order_timestamp() const { return order_timestamp; }
        void set_order_timestamp(std::shared_ptr<std::string> value) { this->order_timestamp = value; }

        std::shared_ptr<std::string> get_product() const { return product; }
        void set_product(std::shared_ptr<std::string> value) { this->product = value; }

        std::shared_ptr<int64_t> get_quantity() const { return quantity; }
        void set_quantity(std::shared_ptr<int64_t> value) { this->quantity = value; }

        std::shared_ptr<std::string> get_trade_id() const { return trade_id; }
        void set_trade_id(std::shared_ptr<std::string> value) { this->trade_id = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_transaction_type() const { return transaction_type; }
        void set_transaction_type(std::shared_ptr<std::string> value) { this->transaction_type = value; }
    };

    class Trades {
        public:
        Trades() = default;
        virtual ~Trades() = default;

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
    void from_json(const json & j,  Trades::Datum & x);
    void to_json(json & j, const  Trades::Datum & x);

    void from_json(const json & j,  Trades::Trades & x);
    void to_json(json & j, const  Trades::Trades & x);

    inline void from_json(const json & j,  Trades::Datum& x) {
        x.set_average_price( Trades::get_optional<double>(j, "average_price"));
        x.set_exchange( Trades::get_optional<std::string>(j, "exchange"));
        x.set_exchange_order_id( Trades::get_optional<std::string>(j, "exchange_order_id"));
        x.set_exchange_timestamp( Trades::get_optional<std::string>(j, "exchange_timestamp"));
        x.set_fill_timestamp( Trades::get_optional<std::string>(j, "fill_timestamp"));
        x.set_instrument_token( Trades::get_optional<int64_t>(j, "instrument_token"));
        x.set_order_id( Trades::get_optional<std::string>(j, "order_id"));
        x.set_order_timestamp( Trades::get_optional<std::string>(j, "order_timestamp"));
        x.set_product( Trades::get_optional<std::string>(j, "product"));
        x.set_quantity( Trades::get_optional<int64_t>(j, "quantity"));
        x.set_trade_id( Trades::get_optional<std::string>(j, "trade_id"));
        x.set_tradingsymbol( Trades::get_optional<std::string>(j, "tradingsymbol"));
        x.set_transaction_type( Trades::get_optional<std::string>(j, "transaction_type"));
    }

    inline void to_json(json & j, const  Trades::Datum & x) {
        j = json::object();
        j["average_price"] = x.get_average_price();
        j["exchange"] = x.get_exchange();
        j["exchange_order_id"] = x.get_exchange_order_id();
        j["exchange_timestamp"] = x.get_exchange_timestamp();
        j["fill_timestamp"] = x.get_fill_timestamp();
        j["instrument_token"] = x.get_instrument_token();
        j["order_id"] = x.get_order_id();
        j["order_timestamp"] = x.get_order_timestamp();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["trade_id"] = x.get_trade_id();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
    }

    inline void from_json(const json & j,  Trades::Trades& x) {
        x.set_data( Trades::get_optional<std::vector< Trades::Datum>>(j, "data"));
        x.set_status( Trades::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const  Trades::Trades & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
