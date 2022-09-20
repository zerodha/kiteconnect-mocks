//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     MfOrdersInfo data = nlohmann::json::parse(jsonString);

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

namespace MfOrdersInfo {
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

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        std::shared_ptr<int64_t> amount;
        std::shared_ptr<int64_t> average_price;
        nlohmann::json exchange_order_id;
        nlohmann::json exchange_timestamp;
        nlohmann::json folio;
        std::shared_ptr<std::string> fund;
        std::shared_ptr<double> last_price;
        std::shared_ptr<std::string> last_price_date;
        std::shared_ptr<std::string> order_id;
        std::shared_ptr<std::string> order_timestamp;
        std::shared_ptr<std::string> placed_by;
        std::shared_ptr<std::string> purchase_type;
        std::shared_ptr<int64_t> quantity;
        nlohmann::json settlement_id;
        std::shared_ptr<std::string> status;
        std::shared_ptr<std::string> status_message;
        nlohmann::json tag;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> transaction_type;
        std::shared_ptr<std::string> variety;

        public:
        std::shared_ptr<int64_t> get_amount() const { return amount; }
        void set_amount(std::shared_ptr<int64_t> value) { this->amount = value; }

        std::shared_ptr<int64_t> get_average_price() const { return average_price; }
        void set_average_price(std::shared_ptr<int64_t> value) { this->average_price = value; }

        const nlohmann::json & get_exchange_order_id() const { return exchange_order_id; }
        nlohmann::json & get_mutable_exchange_order_id() { return exchange_order_id; }
        void set_exchange_order_id(const nlohmann::json & value) { this->exchange_order_id = value; }

        const nlohmann::json & get_exchange_timestamp() const { return exchange_timestamp; }
        nlohmann::json & get_mutable_exchange_timestamp() { return exchange_timestamp; }
        void set_exchange_timestamp(const nlohmann::json & value) { this->exchange_timestamp = value; }

        const nlohmann::json & get_folio() const { return folio; }
        nlohmann::json & get_mutable_folio() { return folio; }
        void set_folio(const nlohmann::json & value) { this->folio = value; }

        std::shared_ptr<std::string> get_fund() const { return fund; }
        void set_fund(std::shared_ptr<std::string> value) { this->fund = value; }

        std::shared_ptr<double> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<double> value) { this->last_price = value; }

        std::shared_ptr<std::string> get_last_price_date() const { return last_price_date; }
        void set_last_price_date(std::shared_ptr<std::string> value) { this->last_price_date = value; }

        std::shared_ptr<std::string> get_order_id() const { return order_id; }
        void set_order_id(std::shared_ptr<std::string> value) { this->order_id = value; }

        std::shared_ptr<std::string> get_order_timestamp() const { return order_timestamp; }
        void set_order_timestamp(std::shared_ptr<std::string> value) { this->order_timestamp = value; }

        std::shared_ptr<std::string> get_placed_by() const { return placed_by; }
        void set_placed_by(std::shared_ptr<std::string> value) { this->placed_by = value; }

        std::shared_ptr<std::string> get_purchase_type() const { return purchase_type; }
        void set_purchase_type(std::shared_ptr<std::string> value) { this->purchase_type = value; }

        std::shared_ptr<int64_t> get_quantity() const { return quantity; }
        void set_quantity(std::shared_ptr<int64_t> value) { this->quantity = value; }

        const nlohmann::json & get_settlement_id() const { return settlement_id; }
        nlohmann::json & get_mutable_settlement_id() { return settlement_id; }
        void set_settlement_id(const nlohmann::json & value) { this->settlement_id = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }

        std::shared_ptr<std::string> get_status_message() const { return status_message; }
        void set_status_message(std::shared_ptr<std::string> value) { this->status_message = value; }

        const nlohmann::json & get_tag() const { return tag; }
        nlohmann::json & get_mutable_tag() { return tag; }
        void set_tag(const nlohmann::json & value) { this->tag = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_transaction_type() const { return transaction_type; }
        void set_transaction_type(std::shared_ptr<std::string> value) { this->transaction_type = value; }

        std::shared_ptr<std::string> get_variety() const { return variety; }
        void set_variety(std::shared_ptr<std::string> value) { this->variety = value; }
    };

    class MfOrdersInfo {
        public:
        MfOrdersInfo() = default;
        virtual ~MfOrdersInfo() = default;

        private:
        std::shared_ptr<Data> data;
        std::shared_ptr<std::string> status;

        public:
        std::shared_ptr<Data> get_data() const { return data; }
        void set_data(std::shared_ptr<Data> value) { this->data = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }
    };
}

namespace nlohmann {
    void from_json(const json & j, MfOrdersInfo::Data & x);
    void to_json(json & j, const MfOrdersInfo::Data & x);

    void from_json(const json & j, MfOrdersInfo::MfOrdersInfo & x);
    void to_json(json & j, const MfOrdersInfo::MfOrdersInfo & x);

    inline void from_json(const json & j, MfOrdersInfo::Data& x) {
        x.set_amount(MfOrdersInfo::get_optional<int64_t>(j, "amount"));
        x.set_average_price(MfOrdersInfo::get_optional<int64_t>(j, "average_price"));
        x.set_exchange_order_id(MfOrdersInfo::get_untyped(j, "exchange_order_id"));
        x.set_exchange_timestamp(MfOrdersInfo::get_untyped(j, "exchange_timestamp"));
        x.set_folio(MfOrdersInfo::get_untyped(j, "folio"));
        x.set_fund(MfOrdersInfo::get_optional<std::string>(j, "fund"));
        x.set_last_price(MfOrdersInfo::get_optional<double>(j, "last_price"));
        x.set_last_price_date(MfOrdersInfo::get_optional<std::string>(j, "last_price_date"));
        x.set_order_id(MfOrdersInfo::get_optional<std::string>(j, "order_id"));
        x.set_order_timestamp(MfOrdersInfo::get_optional<std::string>(j, "order_timestamp"));
        x.set_placed_by(MfOrdersInfo::get_optional<std::string>(j, "placed_by"));
        x.set_purchase_type(MfOrdersInfo::get_optional<std::string>(j, "purchase_type"));
        x.set_quantity(MfOrdersInfo::get_optional<int64_t>(j, "quantity"));
        x.set_settlement_id(MfOrdersInfo::get_untyped(j, "settlement_id"));
        x.set_status(MfOrdersInfo::get_optional<std::string>(j, "status"));
        x.set_status_message(MfOrdersInfo::get_optional<std::string>(j, "status_message"));
        x.set_tag(MfOrdersInfo::get_untyped(j, "tag"));
        x.set_tradingsymbol(MfOrdersInfo::get_optional<std::string>(j, "tradingsymbol"));
        x.set_transaction_type(MfOrdersInfo::get_optional<std::string>(j, "transaction_type"));
        x.set_variety(MfOrdersInfo::get_optional<std::string>(j, "variety"));
    }

    inline void to_json(json & j, const MfOrdersInfo::Data & x) {
        j = json::object();
        j["amount"] = x.get_amount();
        j["average_price"] = x.get_average_price();
        j["exchange_order_id"] = x.get_exchange_order_id();
        j["exchange_timestamp"] = x.get_exchange_timestamp();
        j["folio"] = x.get_folio();
        j["fund"] = x.get_fund();
        j["last_price"] = x.get_last_price();
        j["last_price_date"] = x.get_last_price_date();
        j["order_id"] = x.get_order_id();
        j["order_timestamp"] = x.get_order_timestamp();
        j["placed_by"] = x.get_placed_by();
        j["purchase_type"] = x.get_purchase_type();
        j["quantity"] = x.get_quantity();
        j["settlement_id"] = x.get_settlement_id();
        j["status"] = x.get_status();
        j["status_message"] = x.get_status_message();
        j["tag"] = x.get_tag();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["variety"] = x.get_variety();
    }

    inline void from_json(const json & j, MfOrdersInfo::MfOrdersInfo& x) {
        x.set_data(MfOrdersInfo::get_optional<MfOrdersInfo::Data>(j, "data"));
        x.set_status(MfOrdersInfo::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const MfOrdersInfo::MfOrdersInfo & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
