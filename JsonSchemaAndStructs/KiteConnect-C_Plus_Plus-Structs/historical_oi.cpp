//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     HistoricalOi data = nlohmann::json::parse(jsonString);

#pragma once

#include <boost/variant.hpp>
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

namespace HistoricalOi {
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

    using Candle = boost::variant<double, std::string>;

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        std::shared_ptr<std::vector<std::vector<Candle>>> candles;

        public:
        std::shared_ptr<std::vector<std::vector<Candle>>> get_candles() const { return candles; }
        void set_candles(std::shared_ptr<std::vector<std::vector<Candle>>> value) { this->candles = value; }
    };

    class HistoricalOi {
        public:
        HistoricalOi() = default;
        virtual ~HistoricalOi() = default;

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
    void from_json(const json & j, HistoricalOi::Data & x);
    void to_json(json & j, const HistoricalOi::Data & x);

    void from_json(const json & j, HistoricalOi::HistoricalOi & x);
    void to_json(json & j, const HistoricalOi::HistoricalOi & x);

    void from_json(const json & j, boost::variant<double, std::string> & x);
    void to_json(json & j, const boost::variant<double, std::string> & x);

    inline void from_json(const json & j, HistoricalOi::Data& x) {
        x.set_candles(HistoricalOi::get_optional<std::vector<std::vector<HistoricalOi::Candle>>>(j, "candles"));
    }

    inline void to_json(json & j, const HistoricalOi::Data & x) {
        j = json::object();
        j["candles"] = x.get_candles();
    }

    inline void from_json(const json & j, HistoricalOi::HistoricalOi& x) {
        x.set_data(HistoricalOi::get_optional<HistoricalOi::Data>(j, "data"));
        x.set_status(HistoricalOi::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const HistoricalOi::HistoricalOi & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
    inline void from_json(const json & j, boost::variant<double, std::string> & x) {
        if (j.is_number())
            x = j.get<double>();
        else if (j.is_string())
            x = j.get<std::string>();
        else throw "Could not deserialize";
    }

    inline void to_json(json & j, const boost::variant<double, std::string> & x) {
        switch (x.which()) {
            case 0:
                j = boost::get<double>(x);
                break;
            case 1:
                j = boost::get<std::string>(x);
                break;
            default: throw "Input JSON does not conform to schema";
        }
    }
}
