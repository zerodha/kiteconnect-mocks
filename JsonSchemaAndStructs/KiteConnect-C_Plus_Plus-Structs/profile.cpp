//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Profile data = nlohmann::json::parse(jsonString);

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

namespace Profile {
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

    class Meta {
        public:
        Meta() = default;
        virtual ~Meta() = default;

        private:
        std::shared_ptr<std::string> demat_consent;

        public:
        std::shared_ptr<std::string> get_demat_consent() const { return demat_consent; }
        void set_demat_consent(std::shared_ptr<std::string> value) { this->demat_consent = value; }
    };

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        nlohmann::json avatar_url;
        std::shared_ptr<std::string> broker;
        std::shared_ptr<std::string> email;
        std::shared_ptr<std::vector<std::string>> exchanges;
        std::shared_ptr<Meta> meta;
        std::shared_ptr<std::vector<std::string>> order_types;
        std::shared_ptr<std::vector<std::string>> products;
        std::shared_ptr<std::string> user_id;
        std::shared_ptr<std::string> user_name;
        std::shared_ptr<std::string> user_shortname;
        std::shared_ptr<std::string> user_type;

        public:
        const nlohmann::json & get_avatar_url() const { return avatar_url; }
        nlohmann::json & get_mutable_avatar_url() { return avatar_url; }
        void set_avatar_url(const nlohmann::json & value) { this->avatar_url = value; }

        std::shared_ptr<std::string> get_broker() const { return broker; }
        void set_broker(std::shared_ptr<std::string> value) { this->broker = value; }

        std::shared_ptr<std::string> get_email() const { return email; }
        void set_email(std::shared_ptr<std::string> value) { this->email = value; }

        std::shared_ptr<std::vector<std::string>> get_exchanges() const { return exchanges; }
        void set_exchanges(std::shared_ptr<std::vector<std::string>> value) { this->exchanges = value; }

        std::shared_ptr<Meta> get_meta() const { return meta; }
        void set_meta(std::shared_ptr<Meta> value) { this->meta = value; }

        std::shared_ptr<std::vector<std::string>> get_order_types() const { return order_types; }
        void set_order_types(std::shared_ptr<std::vector<std::string>> value) { this->order_types = value; }

        std::shared_ptr<std::vector<std::string>> get_products() const { return products; }
        void set_products(std::shared_ptr<std::vector<std::string>> value) { this->products = value; }

        std::shared_ptr<std::string> get_user_id() const { return user_id; }
        void set_user_id(std::shared_ptr<std::string> value) { this->user_id = value; }

        std::shared_ptr<std::string> get_user_name() const { return user_name; }
        void set_user_name(std::shared_ptr<std::string> value) { this->user_name = value; }

        std::shared_ptr<std::string> get_user_shortname() const { return user_shortname; }
        void set_user_shortname(std::shared_ptr<std::string> value) { this->user_shortname = value; }

        std::shared_ptr<std::string> get_user_type() const { return user_type; }
        void set_user_type(std::shared_ptr<std::string> value) { this->user_type = value; }
    };

    class Profile {
        public:
        Profile() = default;
        virtual ~Profile() = default;

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
    void from_json(const json & j, Profile::Meta & x);
    void to_json(json & j, const Profile::Meta & x);

    void from_json(const json & j, Profile::Data & x);
    void to_json(json & j, const Profile::Data & x);

    void from_json(const json & j, Profile::Profile & x);
    void to_json(json & j, const Profile::Profile & x);

    inline void from_json(const json & j, Profile::Meta& x) {
        x.set_demat_consent(Profile::get_optional<std::string>(j, "demat_consent"));
    }

    inline void to_json(json & j, const Profile::Meta & x) {
        j = json::object();
        j["demat_consent"] = x.get_demat_consent();
    }

    inline void from_json(const json & j, Profile::Data& x) {
        x.set_avatar_url(Profile::get_untyped(j, "avatar_url"));
        x.set_broker(Profile::get_optional<std::string>(j, "broker"));
        x.set_email(Profile::get_optional<std::string>(j, "email"));
        x.set_exchanges(Profile::get_optional<std::vector<std::string>>(j, "exchanges"));
        x.set_meta(Profile::get_optional<Profile::Meta>(j, "meta"));
        x.set_order_types(Profile::get_optional<std::vector<std::string>>(j, "order_types"));
        x.set_products(Profile::get_optional<std::vector<std::string>>(j, "products"));
        x.set_user_id(Profile::get_optional<std::string>(j, "user_id"));
        x.set_user_name(Profile::get_optional<std::string>(j, "user_name"));
        x.set_user_shortname(Profile::get_optional<std::string>(j, "user_shortname"));
        x.set_user_type(Profile::get_optional<std::string>(j, "user_type"));
    }

    inline void to_json(json & j, const Profile::Data & x) {
        j = json::object();
        j["avatar_url"] = x.get_avatar_url();
        j["broker"] = x.get_broker();
        j["email"] = x.get_email();
        j["exchanges"] = x.get_exchanges();
        j["meta"] = x.get_meta();
        j["order_types"] = x.get_order_types();
        j["products"] = x.get_products();
        j["user_id"] = x.get_user_id();
        j["user_name"] = x.get_user_name();
        j["user_shortname"] = x.get_user_shortname();
        j["user_type"] = x.get_user_type();
    }

    inline void from_json(const json & j, Profile::Profile& x) {
        x.set_data(Profile::get_optional<Profile::Data>(j, "data"));
        x.set_status(Profile::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const Profile::Profile & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
