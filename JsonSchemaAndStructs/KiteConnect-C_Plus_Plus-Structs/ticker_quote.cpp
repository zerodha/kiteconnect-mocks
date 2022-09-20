//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     TickerQuote data = nlohmann::json::parse(jsonString);

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

namespace TickerQuote {
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

    class Ohlc {
        public:
        Ohlc() = default;
        virtual ~Ohlc() = default;

        private:
        std::shared_ptr<int64_t> close;
        std::shared_ptr<int64_t> high;
        std::shared_ptr<int64_t> low;
        std::shared_ptr<int64_t> open;

        public:
        std::shared_ptr<int64_t> get_close() const { return close; }
        void set_close(std::shared_ptr<int64_t> value) { this->close = value; }

        std::shared_ptr<int64_t> get_high() const { return high; }
        void set_high(std::shared_ptr<int64_t> value) { this->high = value; }

        std::shared_ptr<int64_t> get_low() const { return low; }
        void set_low(std::shared_ptr<int64_t> value) { this->low = value; }

        std::shared_ptr<int64_t> get_open() const { return open; }
        void set_open(std::shared_ptr<int64_t> value) { this->open = value; }
    };

    class TriggerRangeElement {
        public:
        TriggerRangeElement() = default;
        virtual ~TriggerRangeElement() = default;

        private:
        std::shared_ptr<double> average_traded_price;
        std::shared_ptr<double> change;
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<int64_t> last_price;
        std::shared_ptr<int64_t> last_traded_quantity;
        std::shared_ptr<std::string> mode;
        std::shared_ptr<Ohlc> ohlc;
        std::shared_ptr<int64_t> total_buy_quantity;
        std::shared_ptr<int64_t> total_sell_quantity;
        std::shared_ptr<bool> tradable;
        std::shared_ptr<int64_t> volume_traded;

        public:
        std::shared_ptr<double> get_average_traded_price() const { return average_traded_price; }
        void set_average_traded_price(std::shared_ptr<double> value) { this->average_traded_price = value; }

        std::shared_ptr<double> get_change() const { return change; }
        void set_change(std::shared_ptr<double> value) { this->change = value; }

        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<int64_t> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<int64_t> value) { this->last_price = value; }

        std::shared_ptr<int64_t> get_last_traded_quantity() const { return last_traded_quantity; }
        void set_last_traded_quantity(std::shared_ptr<int64_t> value) { this->last_traded_quantity = value; }

        std::shared_ptr<std::string> get_mode() const { return mode; }
        void set_mode(std::shared_ptr<std::string> value) { this->mode = value; }

        std::shared_ptr<Ohlc> get_ohlc() const { return ohlc; }
        void set_ohlc(std::shared_ptr<Ohlc> value) { this->ohlc = value; }

        std::shared_ptr<int64_t> get_total_buy_quantity() const { return total_buy_quantity; }
        void set_total_buy_quantity(std::shared_ptr<int64_t> value) { this->total_buy_quantity = value; }

        std::shared_ptr<int64_t> get_total_sell_quantity() const { return total_sell_quantity; }
        void set_total_sell_quantity(std::shared_ptr<int64_t> value) { this->total_sell_quantity = value; }

        std::shared_ptr<bool> get_tradable() const { return tradable; }
        void set_tradable(std::shared_ptr<bool> value) { this->tradable = value; }

        std::shared_ptr<int64_t> get_volume_traded() const { return volume_traded; }
        void set_volume_traded(std::shared_ptr<int64_t> value) { this->volume_traded = value; }
    };

    using TickerQuote = std::vector<TriggerRangeElement>;
}

namespace TickerQuote {
    using TickerQuote = std::vector<TriggerRangeElement>;
}

namespace nlohmann {
    void from_json(const json & j, TickerQuote::Ohlc & x);
    void to_json(json & j, const TickerQuote::Ohlc & x);

    void from_json(const json & j, TickerQuote::TriggerRangeElement & x);
    void to_json(json & j, const TickerQuote::TriggerRangeElement & x);

    inline void from_json(const json & j, TickerQuote::Ohlc& x) {
        x.set_close(TickerQuote::get_optional<int64_t>(j, "close"));
        x.set_high(TickerQuote::get_optional<int64_t>(j, "high"));
        x.set_low(TickerQuote::get_optional<int64_t>(j, "low"));
        x.set_open(TickerQuote::get_optional<int64_t>(j, "open"));
    }

    inline void to_json(json & j, const TickerQuote::Ohlc & x) {
        j = json::object();
        j["close"] = x.get_close();
        j["high"] = x.get_high();
        j["low"] = x.get_low();
        j["open"] = x.get_open();
    }

    inline void from_json(const json & j, TickerQuote::TriggerRangeElement& x) {
        x.set_average_traded_price(TickerQuote::get_optional<double>(j, "average_traded_price"));
        x.set_change(TickerQuote::get_optional<double>(j, "change"));
        x.set_instrument_token(TickerQuote::get_optional<int64_t>(j, "instrument_token"));
        x.set_last_price(TickerQuote::get_optional<int64_t>(j, "last_price"));
        x.set_last_traded_quantity(TickerQuote::get_optional<int64_t>(j, "last_traded_quantity"));
        x.set_mode(TickerQuote::get_optional<std::string>(j, "mode"));
        x.set_ohlc(TickerQuote::get_optional<TickerQuote::Ohlc>(j, "ohlc"));
        x.set_total_buy_quantity(TickerQuote::get_optional<int64_t>(j, "total_buy_quantity"));
        x.set_total_sell_quantity(TickerQuote::get_optional<int64_t>(j, "total_sell_quantity"));
        x.set_tradable(TickerQuote::get_optional<bool>(j, "tradable"));
        x.set_volume_traded(TickerQuote::get_optional<int64_t>(j, "volume_traded"));
    }

    inline void to_json(json & j, const TickerQuote::TriggerRangeElement & x) {
        j = json::object();
        j["average_traded_price"] = x.get_average_traded_price();
        j["change"] = x.get_change();
        j["instrument_token"] = x.get_instrument_token();
        j["last_price"] = x.get_last_price();
        j["last_traded_quantity"] = x.get_last_traded_quantity();
        j["mode"] = x.get_mode();
        j["ohlc"] = x.get_ohlc();
        j["total_buy_quantity"] = x.get_total_buy_quantity();
        j["total_sell_quantity"] = x.get_total_sell_quantity();
        j["tradable"] = x.get_tradable();
        j["volume_traded"] = x.get_volume_traded();
    }
}
