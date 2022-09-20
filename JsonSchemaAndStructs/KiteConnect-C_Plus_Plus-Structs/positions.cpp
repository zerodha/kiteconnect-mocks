//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Positions data = nlohmann::json::parse(jsonString);

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

namespace Positions {
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

    class Day {
        public:
        Day() = default;
        virtual ~Day() = default;

        private:
        std::shared_ptr<double> average_price;
        std::shared_ptr<int64_t> buy_m2_m;
        std::shared_ptr<double> buy_price;
        std::shared_ptr<int64_t> buy_quantity;
        std::shared_ptr<int64_t> buy_value;
        std::shared_ptr<int64_t> close_price;
        std::shared_ptr<double> day_buy_price;
        std::shared_ptr<int64_t> day_buy_quantity;
        std::shared_ptr<int64_t> day_buy_value;
        std::shared_ptr<int64_t> day_sell_price;
        std::shared_ptr<int64_t> day_sell_quantity;
        std::shared_ptr<int64_t> day_sell_value;
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<double> last_price;
        std::shared_ptr<int64_t> m2_m;
        std::shared_ptr<int64_t> multiplier;
        std::shared_ptr<int64_t> overnight_quantity;
        std::shared_ptr<int64_t> pnl;
        std::shared_ptr<std::string> product;
        std::shared_ptr<int64_t> quantity;
        std::shared_ptr<int64_t> realised;
        std::shared_ptr<int64_t> sell_m2_m;
        std::shared_ptr<int64_t> sell_price;
        std::shared_ptr<int64_t> sell_quantity;
        std::shared_ptr<int64_t> sell_value;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<int64_t> unrealised;
        std::shared_ptr<int64_t> value;

        public:
        std::shared_ptr<double> get_average_price() const { return average_price; }
        void set_average_price(std::shared_ptr<double> value) { this->average_price = value; }

        std::shared_ptr<int64_t> get_buy_m2_m() const { return buy_m2_m; }
        void set_buy_m2_m(std::shared_ptr<int64_t> value) { this->buy_m2_m = value; }

        std::shared_ptr<double> get_buy_price() const { return buy_price; }
        void set_buy_price(std::shared_ptr<double> value) { this->buy_price = value; }

        std::shared_ptr<int64_t> get_buy_quantity() const { return buy_quantity; }
        void set_buy_quantity(std::shared_ptr<int64_t> value) { this->buy_quantity = value; }

        std::shared_ptr<int64_t> get_buy_value() const { return buy_value; }
        void set_buy_value(std::shared_ptr<int64_t> value) { this->buy_value = value; }

        std::shared_ptr<int64_t> get_close_price() const { return close_price; }
        void set_close_price(std::shared_ptr<int64_t> value) { this->close_price = value; }

        std::shared_ptr<double> get_day_buy_price() const { return day_buy_price; }
        void set_day_buy_price(std::shared_ptr<double> value) { this->day_buy_price = value; }

        std::shared_ptr<int64_t> get_day_buy_quantity() const { return day_buy_quantity; }
        void set_day_buy_quantity(std::shared_ptr<int64_t> value) { this->day_buy_quantity = value; }

        std::shared_ptr<int64_t> get_day_buy_value() const { return day_buy_value; }
        void set_day_buy_value(std::shared_ptr<int64_t> value) { this->day_buy_value = value; }

        std::shared_ptr<int64_t> get_day_sell_price() const { return day_sell_price; }
        void set_day_sell_price(std::shared_ptr<int64_t> value) { this->day_sell_price = value; }

        std::shared_ptr<int64_t> get_day_sell_quantity() const { return day_sell_quantity; }
        void set_day_sell_quantity(std::shared_ptr<int64_t> value) { this->day_sell_quantity = value; }

        std::shared_ptr<int64_t> get_day_sell_value() const { return day_sell_value; }
        void set_day_sell_value(std::shared_ptr<int64_t> value) { this->day_sell_value = value; }

        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<double> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<double> value) { this->last_price = value; }

        std::shared_ptr<int64_t> get_m2_m() const { return m2_m; }
        void set_m2_m(std::shared_ptr<int64_t> value) { this->m2_m = value; }

        std::shared_ptr<int64_t> get_multiplier() const { return multiplier; }
        void set_multiplier(std::shared_ptr<int64_t> value) { this->multiplier = value; }

        std::shared_ptr<int64_t> get_overnight_quantity() const { return overnight_quantity; }
        void set_overnight_quantity(std::shared_ptr<int64_t> value) { this->overnight_quantity = value; }

        std::shared_ptr<int64_t> get_pnl() const { return pnl; }
        void set_pnl(std::shared_ptr<int64_t> value) { this->pnl = value; }

        std::shared_ptr<std::string> get_product() const { return product; }
        void set_product(std::shared_ptr<std::string> value) { this->product = value; }

        std::shared_ptr<int64_t> get_quantity() const { return quantity; }
        void set_quantity(std::shared_ptr<int64_t> value) { this->quantity = value; }

        std::shared_ptr<int64_t> get_realised() const { return realised; }
        void set_realised(std::shared_ptr<int64_t> value) { this->realised = value; }

        std::shared_ptr<int64_t> get_sell_m2_m() const { return sell_m2_m; }
        void set_sell_m2_m(std::shared_ptr<int64_t> value) { this->sell_m2_m = value; }

        std::shared_ptr<int64_t> get_sell_price() const { return sell_price; }
        void set_sell_price(std::shared_ptr<int64_t> value) { this->sell_price = value; }

        std::shared_ptr<int64_t> get_sell_quantity() const { return sell_quantity; }
        void set_sell_quantity(std::shared_ptr<int64_t> value) { this->sell_quantity = value; }

        std::shared_ptr<int64_t> get_sell_value() const { return sell_value; }
        void set_sell_value(std::shared_ptr<int64_t> value) { this->sell_value = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<int64_t> get_unrealised() const { return unrealised; }
        void set_unrealised(std::shared_ptr<int64_t> value) { this->unrealised = value; }

        std::shared_ptr<int64_t> get_value() const { return value; }
        void set_value(std::shared_ptr<int64_t> value) { this->value = value; }
    };

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        std::shared_ptr<std::vector<Day>> day;
        std::shared_ptr<std::vector<Day>> net;

        public:
        std::shared_ptr<std::vector<Day>> get_day() const { return day; }
        void set_day(std::shared_ptr<std::vector<Day>> value) { this->day = value; }

        std::shared_ptr<std::vector<Day>> get_net() const { return net; }
        void set_net(std::shared_ptr<std::vector<Day>> value) { this->net = value; }
    };

    class Positions {
        public:
        Positions() = default;
        virtual ~Positions() = default;

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
    void from_json(const json & j, Positions::Day & x);
    void to_json(json & j, const Positions::Day & x);

    void from_json(const json & j, Positions::Data & x);
    void to_json(json & j, const Positions::Data & x);

    void from_json(const json & j, Positions::Positions & x);
    void to_json(json & j, const Positions::Positions & x);

    inline void from_json(const json & j, Positions::Day& x) {
        x.set_average_price(Positions::get_optional<double>(j, "average_price"));
        x.set_buy_m2_m(Positions::get_optional<int64_t>(j, "buy_m2m"));
        x.set_buy_price(Positions::get_optional<double>(j, "buy_price"));
        x.set_buy_quantity(Positions::get_optional<int64_t>(j, "buy_quantity"));
        x.set_buy_value(Positions::get_optional<int64_t>(j, "buy_value"));
        x.set_close_price(Positions::get_optional<int64_t>(j, "close_price"));
        x.set_day_buy_price(Positions::get_optional<double>(j, "day_buy_price"));
        x.set_day_buy_quantity(Positions::get_optional<int64_t>(j, "day_buy_quantity"));
        x.set_day_buy_value(Positions::get_optional<int64_t>(j, "day_buy_value"));
        x.set_day_sell_price(Positions::get_optional<int64_t>(j, "day_sell_price"));
        x.set_day_sell_quantity(Positions::get_optional<int64_t>(j, "day_sell_quantity"));
        x.set_day_sell_value(Positions::get_optional<int64_t>(j, "day_sell_value"));
        x.set_exchange(Positions::get_optional<std::string>(j, "exchange"));
        x.set_instrument_token(Positions::get_optional<int64_t>(j, "instrument_token"));
        x.set_last_price(Positions::get_optional<double>(j, "last_price"));
        x.set_m2_m(Positions::get_optional<int64_t>(j, "m2m"));
        x.set_multiplier(Positions::get_optional<int64_t>(j, "multiplier"));
        x.set_overnight_quantity(Positions::get_optional<int64_t>(j, "overnight_quantity"));
        x.set_pnl(Positions::get_optional<int64_t>(j, "pnl"));
        x.set_product(Positions::get_optional<std::string>(j, "product"));
        x.set_quantity(Positions::get_optional<int64_t>(j, "quantity"));
        x.set_realised(Positions::get_optional<int64_t>(j, "realised"));
        x.set_sell_m2_m(Positions::get_optional<int64_t>(j, "sell_m2m"));
        x.set_sell_price(Positions::get_optional<int64_t>(j, "sell_price"));
        x.set_sell_quantity(Positions::get_optional<int64_t>(j, "sell_quantity"));
        x.set_sell_value(Positions::get_optional<int64_t>(j, "sell_value"));
        x.set_tradingsymbol(Positions::get_optional<std::string>(j, "tradingsymbol"));
        x.set_unrealised(Positions::get_optional<int64_t>(j, "unrealised"));
        x.set_value(Positions::get_optional<int64_t>(j, "value"));
    }

    inline void to_json(json & j, const Positions::Day & x) {
        j = json::object();
        j["average_price"] = x.get_average_price();
        j["buy_m2m"] = x.get_buy_m2_m();
        j["buy_price"] = x.get_buy_price();
        j["buy_quantity"] = x.get_buy_quantity();
        j["buy_value"] = x.get_buy_value();
        j["close_price"] = x.get_close_price();
        j["day_buy_price"] = x.get_day_buy_price();
        j["day_buy_quantity"] = x.get_day_buy_quantity();
        j["day_buy_value"] = x.get_day_buy_value();
        j["day_sell_price"] = x.get_day_sell_price();
        j["day_sell_quantity"] = x.get_day_sell_quantity();
        j["day_sell_value"] = x.get_day_sell_value();
        j["exchange"] = x.get_exchange();
        j["instrument_token"] = x.get_instrument_token();
        j["last_price"] = x.get_last_price();
        j["m2m"] = x.get_m2_m();
        j["multiplier"] = x.get_multiplier();
        j["overnight_quantity"] = x.get_overnight_quantity();
        j["pnl"] = x.get_pnl();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["realised"] = x.get_realised();
        j["sell_m2m"] = x.get_sell_m2_m();
        j["sell_price"] = x.get_sell_price();
        j["sell_quantity"] = x.get_sell_quantity();
        j["sell_value"] = x.get_sell_value();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["unrealised"] = x.get_unrealised();
        j["value"] = x.get_value();
    }

    inline void from_json(const json & j, Positions::Data& x) {
        x.set_day(Positions::get_optional<std::vector<Positions::Day>>(j, "day"));
        x.set_net(Positions::get_optional<std::vector<Positions::Day>>(j, "net"));
    }

    inline void to_json(json & j, const Positions::Data & x) {
        j = json::object();
        j["day"] = x.get_day();
        j["net"] = x.get_net();
    }

    inline void from_json(const json & j, Positions::Positions& x) {
        x.set_data(Positions::get_optional<Positions::Data>(j, "data"));
        x.set_status(Positions::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const Positions::Positions & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
