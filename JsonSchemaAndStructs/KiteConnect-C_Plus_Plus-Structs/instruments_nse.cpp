//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     InstrumentsNse data = nlohmann::json::parse(jsonString);

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

namespace InstrumentsNse {
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

    enum class Exchange : int { NSE };

    enum class InstrumentType : int { EQ };

    class InstrumentsNseElement {
        public:
        InstrumentsNseElement() = default;
        virtual ~InstrumentsNseElement() = default;

        private:
        std::shared_ptr<Exchange> exchange;
        std::shared_ptr<int64_t> exchange_token;
        std::shared_ptr<std::string> expiry;
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<InstrumentType> instrument_type;
        std::shared_ptr<int64_t> last_price;
        std::shared_ptr<int64_t> lot_size;
        std::shared_ptr<std::string> name;
        std::shared_ptr<Exchange> segment;
        std::shared_ptr<int64_t> strike;
        std::shared_ptr<double> tick_size;
        std::shared_ptr<std::string> tradingsymbol;

        public:
        std::shared_ptr<Exchange> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<Exchange> value) { this->exchange = value; }

        std::shared_ptr<int64_t> get_exchange_token() const { return exchange_token; }
        void set_exchange_token(std::shared_ptr<int64_t> value) { this->exchange_token = value; }

        std::shared_ptr<std::string> get_expiry() const { return expiry; }
        void set_expiry(std::shared_ptr<std::string> value) { this->expiry = value; }

        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<InstrumentType> get_instrument_type() const { return instrument_type; }
        void set_instrument_type(std::shared_ptr<InstrumentType> value) { this->instrument_type = value; }

        std::shared_ptr<int64_t> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<int64_t> value) { this->last_price = value; }

        std::shared_ptr<int64_t> get_lot_size() const { return lot_size; }
        void set_lot_size(std::shared_ptr<int64_t> value) { this->lot_size = value; }

        std::shared_ptr<std::string> get_name() const { return name; }
        void set_name(std::shared_ptr<std::string> value) { this->name = value; }

        std::shared_ptr<Exchange> get_segment() const { return segment; }
        void set_segment(std::shared_ptr<Exchange> value) { this->segment = value; }

        std::shared_ptr<int64_t> get_strike() const { return strike; }
        void set_strike(std::shared_ptr<int64_t> value) { this->strike = value; }

        std::shared_ptr<double> get_tick_size() const { return tick_size; }
        void set_tick_size(std::shared_ptr<double> value) { this->tick_size = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }
    };

    using InstrumentsNse = std::vector<InstrumentsNseElement>;
}

namespace InstrumentsNse {
    using InstrumentsNse = std::vector<InstrumentsNseElement>;
}

namespace nlohmann {
    void from_json(const json & j, InstrumentsNse::InstrumentsNseElement & x);
    void to_json(json & j, const InstrumentsNse::InstrumentsNseElement & x);

    void from_json(const json & j, InstrumentsNse::Exchange & x);
    void to_json(json & j, const InstrumentsNse::Exchange & x);

    void from_json(const json & j, InstrumentsNse::InstrumentType & x);
    void to_json(json & j, const InstrumentsNse::InstrumentType & x);

    inline void from_json(const json & j, InstrumentsNse::InstrumentsNseElement& x) {
        x.set_exchange(InstrumentsNse::get_optional<InstrumentsNse::Exchange>(j, "exchange"));
        x.set_exchange_token(InstrumentsNse::get_optional<int64_t>(j, "exchange_token"));
        x.set_expiry(InstrumentsNse::get_optional<std::string>(j, "expiry"));
        x.set_instrument_token(InstrumentsNse::get_optional<int64_t>(j, "instrument_token"));
        x.set_instrument_type(InstrumentsNse::get_optional<InstrumentsNse::InstrumentType>(j, "instrument_type"));
        x.set_last_price(InstrumentsNse::get_optional<int64_t>(j, "last_price"));
        x.set_lot_size(InstrumentsNse::get_optional<int64_t>(j, "lot_size"));
        x.set_name(InstrumentsNse::get_optional<std::string>(j, "name"));
        x.set_segment(InstrumentsNse::get_optional<InstrumentsNse::Exchange>(j, "segment"));
        x.set_strike(InstrumentsNse::get_optional<int64_t>(j, "strike"));
        x.set_tick_size(InstrumentsNse::get_optional<double>(j, "tick_size"));
        x.set_tradingsymbol(InstrumentsNse::get_optional<std::string>(j, "tradingsymbol"));
    }

    inline void to_json(json & j, const InstrumentsNse::InstrumentsNseElement & x) {
        j = json::object();
        j["exchange"] = x.get_exchange();
        j["exchange_token"] = x.get_exchange_token();
        j["expiry"] = x.get_expiry();
        j["instrument_token"] = x.get_instrument_token();
        j["instrument_type"] = x.get_instrument_type();
        j["last_price"] = x.get_last_price();
        j["lot_size"] = x.get_lot_size();
        j["name"] = x.get_name();
        j["segment"] = x.get_segment();
        j["strike"] = x.get_strike();
        j["tick_size"] = x.get_tick_size();
        j["tradingsymbol"] = x.get_tradingsymbol();
    }

    inline void from_json(const json & j, InstrumentsNse::Exchange & x) {
        if (j == "NSE") x = InstrumentsNse::Exchange::NSE;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const InstrumentsNse::Exchange & x) {
        switch (x) {
            case InstrumentsNse::Exchange::NSE: j = "NSE"; break;
            default: throw "This should not happen";
        }
    }

    inline void from_json(const json & j, InstrumentsNse::InstrumentType & x) {
        if (j == "EQ") x = InstrumentsNse::InstrumentType::EQ;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const InstrumentsNse::InstrumentType & x) {
        switch (x) {
            case InstrumentsNse::InstrumentType::EQ: j = "EQ"; break;
            default: throw "This should not happen";
        }
    }
}
