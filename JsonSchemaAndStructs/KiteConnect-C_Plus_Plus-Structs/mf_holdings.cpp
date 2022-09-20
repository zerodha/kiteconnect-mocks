//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     MfHoldings data = nlohmann::json::parse(jsonString);

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

namespace MfHoldings {
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
        std::shared_ptr<std::string> folio;
        std::shared_ptr<std::string> fund;
        std::shared_ptr<double> last_price;
        std::shared_ptr<std::string> last_price_date;
        std::shared_ptr<int64_t> pledged_quantity;
        std::shared_ptr<int64_t> pnl;
        std::shared_ptr<double> quantity;
        std::shared_ptr<std::string> tradingsymbol;

        public:
        std::shared_ptr<double> get_average_price() const { return average_price; }
        void set_average_price(std::shared_ptr<double> value) { this->average_price = value; }

        std::shared_ptr<std::string> get_folio() const { return folio; }
        void set_folio(std::shared_ptr<std::string> value) { this->folio = value; }

        std::shared_ptr<std::string> get_fund() const { return fund; }
        void set_fund(std::shared_ptr<std::string> value) { this->fund = value; }

        std::shared_ptr<double> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<double> value) { this->last_price = value; }

        std::shared_ptr<std::string> get_last_price_date() const { return last_price_date; }
        void set_last_price_date(std::shared_ptr<std::string> value) { this->last_price_date = value; }

        std::shared_ptr<int64_t> get_pledged_quantity() const { return pledged_quantity; }
        void set_pledged_quantity(std::shared_ptr<int64_t> value) { this->pledged_quantity = value; }

        std::shared_ptr<int64_t> get_pnl() const { return pnl; }
        void set_pnl(std::shared_ptr<int64_t> value) { this->pnl = value; }

        std::shared_ptr<double> get_quantity() const { return quantity; }
        void set_quantity(std::shared_ptr<double> value) { this->quantity = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }
    };

    class MfHoldings {
        public:
        MfHoldings() = default;
        virtual ~MfHoldings() = default;

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
    void from_json(const json & j, MfHoldings::Datum & x);
    void to_json(json & j, const MfHoldings::Datum & x);

    void from_json(const json & j, MfHoldings::MfHoldings & x);
    void to_json(json & j, const MfHoldings::MfHoldings & x);

    inline void from_json(const json & j, MfHoldings::Datum& x) {
        x.set_average_price(MfHoldings::get_optional<double>(j, "average_price"));
        x.set_folio(MfHoldings::get_optional<std::string>(j, "folio"));
        x.set_fund(MfHoldings::get_optional<std::string>(j, "fund"));
        x.set_last_price(MfHoldings::get_optional<double>(j, "last_price"));
        x.set_last_price_date(MfHoldings::get_optional<std::string>(j, "last_price_date"));
        x.set_pledged_quantity(MfHoldings::get_optional<int64_t>(j, "pledged_quantity"));
        x.set_pnl(MfHoldings::get_optional<int64_t>(j, "pnl"));
        x.set_quantity(MfHoldings::get_optional<double>(j, "quantity"));
        x.set_tradingsymbol(MfHoldings::get_optional<std::string>(j, "tradingsymbol"));
    }

    inline void to_json(json & j, const MfHoldings::Datum & x) {
        j = json::object();
        j["average_price"] = x.get_average_price();
        j["folio"] = x.get_folio();
        j["fund"] = x.get_fund();
        j["last_price"] = x.get_last_price();
        j["last_price_date"] = x.get_last_price_date();
        j["pledged_quantity"] = x.get_pledged_quantity();
        j["pnl"] = x.get_pnl();
        j["quantity"] = x.get_quantity();
        j["tradingsymbol"] = x.get_tradingsymbol();
    }

    inline void from_json(const json & j, MfHoldings::MfHoldings& x) {
        x.set_data(MfHoldings::get_optional<std::vector<MfHoldings::Datum>>(j, "data"));
        x.set_status(MfHoldings::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const MfHoldings::MfHoldings & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
