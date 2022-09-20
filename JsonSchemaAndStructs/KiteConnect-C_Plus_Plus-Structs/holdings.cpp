//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Holdings data = nlohmann::json::parse(jsonString);

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

namespace Holdings {
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
        std::shared_ptr<std::string> authorised_date;
        std::shared_ptr<int64_t> authorised_quantity;
        std::shared_ptr<double> average_price;
        std::shared_ptr<double> close_price;
        std::shared_ptr<int64_t> collateral_quantity;
        std::shared_ptr<std::string> collateral_type;
        std::shared_ptr<double> day_change;
        std::shared_ptr<double> day_change_percentage;
        std::shared_ptr<bool> discrepancy;
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<std::string> isin;
        std::shared_ptr<double> last_price;
        std::shared_ptr<int64_t> opening_quantity;
        std::shared_ptr<double> pnl;
        std::shared_ptr<int64_t> price;
        std::shared_ptr<std::string> product;
        std::shared_ptr<int64_t> quantity;
        std::shared_ptr<int64_t> realised_quantity;
        std::shared_ptr<int64_t> t1_quantity;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<int64_t> used_quantity;

        public:
        std::shared_ptr<std::string> get_authorised_date() const { return authorised_date; }
        void set_authorised_date(std::shared_ptr<std::string> value) { this->authorised_date = value; }

        std::shared_ptr<int64_t> get_authorised_quantity() const { return authorised_quantity; }
        void set_authorised_quantity(std::shared_ptr<int64_t> value) { this->authorised_quantity = value; }

        std::shared_ptr<double> get_average_price() const { return average_price; }
        void set_average_price(std::shared_ptr<double> value) { this->average_price = value; }

        std::shared_ptr<double> get_close_price() const { return close_price; }
        void set_close_price(std::shared_ptr<double> value) { this->close_price = value; }

        std::shared_ptr<int64_t> get_collateral_quantity() const { return collateral_quantity; }
        void set_collateral_quantity(std::shared_ptr<int64_t> value) { this->collateral_quantity = value; }

        std::shared_ptr<std::string> get_collateral_type() const { return collateral_type; }
        void set_collateral_type(std::shared_ptr<std::string> value) { this->collateral_type = value; }

        std::shared_ptr<double> get_day_change() const { return day_change; }
        void set_day_change(std::shared_ptr<double> value) { this->day_change = value; }

        std::shared_ptr<double> get_day_change_percentage() const { return day_change_percentage; }
        void set_day_change_percentage(std::shared_ptr<double> value) { this->day_change_percentage = value; }

        std::shared_ptr<bool> get_discrepancy() const { return discrepancy; }
        void set_discrepancy(std::shared_ptr<bool> value) { this->discrepancy = value; }

        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<std::string> get_isin() const { return isin; }
        void set_isin(std::shared_ptr<std::string> value) { this->isin = value; }

        std::shared_ptr<double> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<double> value) { this->last_price = value; }

        std::shared_ptr<int64_t> get_opening_quantity() const { return opening_quantity; }
        void set_opening_quantity(std::shared_ptr<int64_t> value) { this->opening_quantity = value; }

        std::shared_ptr<double> get_pnl() const { return pnl; }
        void set_pnl(std::shared_ptr<double> value) { this->pnl = value; }

        std::shared_ptr<int64_t> get_price() const { return price; }
        void set_price(std::shared_ptr<int64_t> value) { this->price = value; }

        std::shared_ptr<std::string> get_product() const { return product; }
        void set_product(std::shared_ptr<std::string> value) { this->product = value; }

        std::shared_ptr<int64_t> get_quantity() const { return quantity; }
        void set_quantity(std::shared_ptr<int64_t> value) { this->quantity = value; }

        std::shared_ptr<int64_t> get_realised_quantity() const { return realised_quantity; }
        void set_realised_quantity(std::shared_ptr<int64_t> value) { this->realised_quantity = value; }

        std::shared_ptr<int64_t> get_t1_quantity() const { return t1_quantity; }
        void set_t1_quantity(std::shared_ptr<int64_t> value) { this->t1_quantity = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<int64_t> get_used_quantity() const { return used_quantity; }
        void set_used_quantity(std::shared_ptr<int64_t> value) { this->used_quantity = value; }
    };

    class Holdings {
        public:
        Holdings() = default;
        virtual ~Holdings() = default;

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
    void from_json(const json & j, Holdings::Datum & x);
    void to_json(json & j, const Holdings::Datum & x);

    void from_json(const json & j, Holdings::Holdings & x);
    void to_json(json & j, const Holdings::Holdings & x);

    inline void from_json(const json & j, Holdings::Datum& x) {
        x.set_authorised_date(Holdings::get_optional<std::string>(j, "authorised_date"));
        x.set_authorised_quantity(Holdings::get_optional<int64_t>(j, "authorised_quantity"));
        x.set_average_price(Holdings::get_optional<double>(j, "average_price"));
        x.set_close_price(Holdings::get_optional<double>(j, "close_price"));
        x.set_collateral_quantity(Holdings::get_optional<int64_t>(j, "collateral_quantity"));
        x.set_collateral_type(Holdings::get_optional<std::string>(j, "collateral_type"));
        x.set_day_change(Holdings::get_optional<double>(j, "day_change"));
        x.set_day_change_percentage(Holdings::get_optional<double>(j, "day_change_percentage"));
        x.set_discrepancy(Holdings::get_optional<bool>(j, "discrepancy"));
        x.set_exchange(Holdings::get_optional<std::string>(j, "exchange"));
        x.set_instrument_token(Holdings::get_optional<int64_t>(j, "instrument_token"));
        x.set_isin(Holdings::get_optional<std::string>(j, "isin"));
        x.set_last_price(Holdings::get_optional<double>(j, "last_price"));
        x.set_opening_quantity(Holdings::get_optional<int64_t>(j, "opening_quantity"));
        x.set_pnl(Holdings::get_optional<double>(j, "pnl"));
        x.set_price(Holdings::get_optional<int64_t>(j, "price"));
        x.set_product(Holdings::get_optional<std::string>(j, "product"));
        x.set_quantity(Holdings::get_optional<int64_t>(j, "quantity"));
        x.set_realised_quantity(Holdings::get_optional<int64_t>(j, "realised_quantity"));
        x.set_t1_quantity(Holdings::get_optional<int64_t>(j, "t1_quantity"));
        x.set_tradingsymbol(Holdings::get_optional<std::string>(j, "tradingsymbol"));
        x.set_used_quantity(Holdings::get_optional<int64_t>(j, "used_quantity"));
    }

    inline void to_json(json & j, const Holdings::Datum & x) {
        j = json::object();
        j["authorised_date"] = x.get_authorised_date();
        j["authorised_quantity"] = x.get_authorised_quantity();
        j["average_price"] = x.get_average_price();
        j["close_price"] = x.get_close_price();
        j["collateral_quantity"] = x.get_collateral_quantity();
        j["collateral_type"] = x.get_collateral_type();
        j["day_change"] = x.get_day_change();
        j["day_change_percentage"] = x.get_day_change_percentage();
        j["discrepancy"] = x.get_discrepancy();
        j["exchange"] = x.get_exchange();
        j["instrument_token"] = x.get_instrument_token();
        j["isin"] = x.get_isin();
        j["last_price"] = x.get_last_price();
        j["opening_quantity"] = x.get_opening_quantity();
        j["pnl"] = x.get_pnl();
        j["price"] = x.get_price();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["realised_quantity"] = x.get_realised_quantity();
        j["t1_quantity"] = x.get_t1_quantity();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["used_quantity"] = x.get_used_quantity();
    }

    inline void from_json(const json & j, Holdings::Holdings& x) {
        x.set_data(Holdings::get_optional<std::vector<Holdings::Datum>>(j, "data"));
        x.set_status(Holdings::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const Holdings::Holdings & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
