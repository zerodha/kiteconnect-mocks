//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Ohlc data = nlohmann::json::parse(jsonString);

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

namespace Ohlc {
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

    class OhlcClass {
        public:
        OhlcClass() = default;
        virtual ~OhlcClass() = default;

        private:
        std::shared_ptr<double> close;
        std::shared_ptr<double> high;
        std::shared_ptr<double> low;
        std::shared_ptr<double> open;

        public:
        std::shared_ptr<double> get_close() const { return close; }
        void set_close(std::shared_ptr<double> value) { this->close = value; }

        std::shared_ptr<double> get_high() const { return high; }
        void set_high(std::shared_ptr<double> value) { this->high = value; }

        std::shared_ptr<double> get_low() const { return low; }
        void set_low(std::shared_ptr<double> value) { this->low = value; }

        std::shared_ptr<double> get_open() const { return open; }
        void set_open(std::shared_ptr<double> value) { this->open = value; }
    };

    class Datum {
        public:
        Datum() = default;
        virtual ~Datum() = default;

        private:
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<int64_t> last_price;
        std::shared_ptr<OhlcClass> ohlc;

        public:
        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<int64_t> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<int64_t> value) { this->last_price = value; }

        std::shared_ptr<OhlcClass> get_ohlc() const { return ohlc; }
        void set_ohlc(std::shared_ptr<OhlcClass> value) { this->ohlc = value; }
    };

    class Ohlc {
        public:
        Ohlc() = default;
        virtual ~Ohlc() = default;

        private:
        std::shared_ptr<std::map<std::string, Datum>> data;
        std::shared_ptr<std::string> status;

        public:
        std::shared_ptr<std::map<std::string, Datum>> get_data() const { return data; }
        void set_data(std::shared_ptr<std::map<std::string, Datum>> value) { this->data = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }
    };
}

namespace nlohmann {
    void from_json(const json & j, Ohlc::OhlcClass & x);
    void to_json(json & j, const Ohlc::OhlcClass & x);

    void from_json(const json & j, Ohlc::Datum & x);
    void to_json(json & j, const Ohlc::Datum & x);

    void from_json(const json & j, Ohlc::Ohlc & x);
    void to_json(json & j, const Ohlc::Ohlc & x);

    inline void from_json(const json & j, Ohlc::OhlcClass& x) {
        x.set_close(Ohlc::get_optional<double>(j, "close"));
        x.set_high(Ohlc::get_optional<double>(j, "high"));
        x.set_low(Ohlc::get_optional<double>(j, "low"));
        x.set_open(Ohlc::get_optional<double>(j, "open"));
    }

    inline void to_json(json & j, const Ohlc::OhlcClass & x) {
        j = json::object();
        j["close"] = x.get_close();
        j["high"] = x.get_high();
        j["low"] = x.get_low();
        j["open"] = x.get_open();
    }

    inline void from_json(const json & j, Ohlc::Datum& x) {
        x.set_instrument_token(Ohlc::get_optional<int64_t>(j, "instrument_token"));
        x.set_last_price(Ohlc::get_optional<int64_t>(j, "last_price"));
        x.set_ohlc(Ohlc::get_optional<Ohlc::OhlcClass>(j, "ohlc"));
    }

    inline void to_json(json & j, const Ohlc::Datum & x) {
        j = json::object();
        j["instrument_token"] = x.get_instrument_token();
        j["last_price"] = x.get_last_price();
        j["ohlc"] = x.get_ohlc();
    }

    inline void from_json(const json & j, Ohlc::Ohlc& x) {
        x.set_data(Ohlc::get_optional<std::map<std::string, Ohlc::Datum>>(j, "data"));
        x.set_status(Ohlc::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const Ohlc::Ohlc & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
