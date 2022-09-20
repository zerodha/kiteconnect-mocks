//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     GttDeleteOrder data = nlohmann::json::parse(jsonString);

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

namespace GttDeleteOrder {
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
        std::shared_ptr<int64_t> trigger_id;

        public:
        std::shared_ptr<int64_t> get_trigger_id() const { return trigger_id; }
        void set_trigger_id(std::shared_ptr<int64_t> value) { this->trigger_id = value; }
    };

    class GttDeleteOrder {
        public:
        GttDeleteOrder() = default;
        virtual ~GttDeleteOrder() = default;

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
    void from_json(const json & j, GttDeleteOrder::Data & x);
    void to_json(json & j, const GttDeleteOrder::Data & x);

    void from_json(const json & j, GttDeleteOrder::GttDeleteOrder & x);
    void to_json(json & j, const GttDeleteOrder::GttDeleteOrder & x);

    inline void from_json(const json & j, GttDeleteOrder::Data& x) {
        x.set_trigger_id(GttDeleteOrder::get_optional<int64_t>(j, "trigger_id"));
    }

    inline void to_json(json & j, const GttDeleteOrder::Data & x) {
        j = json::object();
        j["trigger_id"] = x.get_trigger_id();
    }

    inline void from_json(const json & j, GttDeleteOrder::GttDeleteOrder& x) {
        x.set_data(GttDeleteOrder::get_optional<GttDeleteOrder::Data>(j, "data"));
        x.set_status(GttDeleteOrder::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const GttDeleteOrder::GttDeleteOrder & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
