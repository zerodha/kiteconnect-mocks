//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     TriggerRange data = nlohmann::json::parse(jsonString);

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

namespace TriggerRange {
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
        std::shared_ptr<int64_t> instrument_token;
        std::shared_ptr<double> lower;
        std::shared_ptr<double> upper;

        public:
        std::shared_ptr<int64_t> get_instrument_token() const { return instrument_token; }
        void set_instrument_token(std::shared_ptr<int64_t> value) { this->instrument_token = value; }

        std::shared_ptr<double> get_lower() const { return lower; }
        void set_lower(std::shared_ptr<double> value) { this->lower = value; }

        std::shared_ptr<double> get_upper() const { return upper; }
        void set_upper(std::shared_ptr<double> value) { this->upper = value; }
    };

    class TriggerRange {
        public:
        TriggerRange() = default;
        virtual ~TriggerRange() = default;

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
    void from_json(const json & j, TriggerRange::Datum & x);
    void to_json(json & j, const TriggerRange::Datum & x);

    void from_json(const json & j, TriggerRange::TriggerRange & x);
    void to_json(json & j, const TriggerRange::TriggerRange & x);

    inline void from_json(const json & j, TriggerRange::Datum& x) {
        x.set_instrument_token(TriggerRange::get_optional<int64_t>(j, "instrument_token"));
        x.set_lower(TriggerRange::get_optional<double>(j, "lower"));
        x.set_upper(TriggerRange::get_optional<double>(j, "upper"));
    }

    inline void to_json(json & j, const TriggerRange::Datum & x) {
        j = json::object();
        j["instrument_token"] = x.get_instrument_token();
        j["lower"] = x.get_lower();
        j["upper"] = x.get_upper();
    }

    inline void from_json(const json & j, TriggerRange::TriggerRange& x) {
        x.set_data(TriggerRange::get_optional<std::map<std::string, TriggerRange::Datum>>(j, "data"));
        x.set_status(TriggerRange::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const TriggerRange::TriggerRange & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
