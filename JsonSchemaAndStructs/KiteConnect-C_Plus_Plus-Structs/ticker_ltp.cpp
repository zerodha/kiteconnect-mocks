//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     TickerLtp data = nlohmann::json::parse(jsonString);

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

namespace TickerLtp {
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

    class TriggerRangeElement {
        public:
        TriggerRangeElement() = default;
        virtual ~TriggerRangeElement() = default;

        private:
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<int64_t> last_price;
        std::shared_ptr<std::string> mode;
        std::shared_ptr<bool> tradable;

        public:
        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<int64_t> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<int64_t> value) { this->last_price = value; }

        std::shared_ptr<std::string> get_mode() const { return mode; }
        void set_mode(std::shared_ptr<std::string> value) { this->mode = value; }

        std::shared_ptr<bool> get_tradable() const { return tradable; }
        void set_tradable(std::shared_ptr<bool> value) { this->tradable = value; }
    };

    using TickerLtp = std::vector<TriggerRangeElement>;
}

namespace TickerLtp {
    using TickerLtp = std::vector<TriggerRangeElement>;
}

namespace nlohmann {
    void from_json(const json & j, TickerLtp::TriggerRangeElement & x);
    void to_json(json & j, const TickerLtp::TriggerRangeElement & x);

    inline void from_json(const json & j, TickerLtp::TriggerRangeElement& x) {
        x.set_instrument_token(TickerLtp::get_optional<int64_t>(j, "instrument_token"));
        x.set_last_price(TickerLtp::get_optional<int64_t>(j, "last_price"));
        x.set_mode(TickerLtp::get_optional<std::string>(j, "mode"));
        x.set_tradable(TickerLtp::get_optional<bool>(j, "tradable"));
    }

    inline void to_json(json & j, const TickerLtp::TriggerRangeElement & x) {
        j = json::object();
        j["instrument_token"] = x.get_instrument_token();
        j["last_price"] = x.get_last_price();
        j["mode"] = x.get_mode();
        j["tradable"] = x.get_tradable();
    }
}
