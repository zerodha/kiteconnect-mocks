//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     GenerateSession data = nlohmann::json::parse(jsonString);

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

namespace GenerateSession {
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
        std::shared_ptr<std::string> access_token;
        std::shared_ptr<std::string> api_key;
        std::shared_ptr<std::string> avatar_url;
        std::shared_ptr<std::string> broker;
        std::shared_ptr<std::string> email;
        std::shared_ptr<std::string> enctoken;
        std::shared_ptr<std::vector<std::string>> exchanges;
        std::shared_ptr<std::string> login_time;
        std::shared_ptr<Meta> meta;
        std::shared_ptr<std::vector<std::string>> order_types;
        std::shared_ptr<std::vector<std::string>> products;
        std::shared_ptr<std::string> public_token;
        std::shared_ptr<std::string> refresh_token;
        std::shared_ptr<std::string> silo;
        std::shared_ptr<std::string> user_id;
        std::shared_ptr<std::string> user_name;
        std::shared_ptr<std::string> user_shortname;
        std::shared_ptr<std::string> user_type;

        public:
        std::shared_ptr<std::string> get_access_token() const { return access_token; }
        void set_access_token(std::shared_ptr<std::string> value) { this->access_token = value; }

        std::shared_ptr<std::string> get_api_key() const { return api_key; }
        void set_api_key(std::shared_ptr<std::string> value) { this->api_key = value; }

        std::shared_ptr<std::string> get_avatar_url() const { return avatar_url; }
        void set_avatar_url(std::shared_ptr<std::string> value) { this->avatar_url = value; }

        std::shared_ptr<std::string> get_broker() const { return broker; }
        void set_broker(std::shared_ptr<std::string> value) { this->broker = value; }

        std::shared_ptr<std::string> get_email() const { return email; }
        void set_email(std::shared_ptr<std::string> value) { this->email = value; }

        std::shared_ptr<std::string> get_enctoken() const { return enctoken; }
        void set_enctoken(std::shared_ptr<std::string> value) { this->enctoken = value; }

        std::shared_ptr<std::vector<std::string>> get_exchanges() const { return exchanges; }
        void set_exchanges(std::shared_ptr<std::vector<std::string>> value) { this->exchanges = value; }

        std::shared_ptr<std::string> get_login_time() const { return login_time; }
        void set_login_time(std::shared_ptr<std::string> value) { this->login_time = value; }

        std::shared_ptr<Meta> get_meta() const { return meta; }
        void set_meta(std::shared_ptr<Meta> value) { this->meta = value; }

        std::shared_ptr<std::vector<std::string>> get_order_types() const { return order_types; }
        void set_order_types(std::shared_ptr<std::vector<std::string>> value) { this->order_types = value; }

        std::shared_ptr<std::vector<std::string>> get_products() const { return products; }
        void set_products(std::shared_ptr<std::vector<std::string>> value) { this->products = value; }

        std::shared_ptr<std::string> get_public_token() const { return public_token; }
        void set_public_token(std::shared_ptr<std::string> value) { this->public_token = value; }

        std::shared_ptr<std::string> get_refresh_token() const { return refresh_token; }
        void set_refresh_token(std::shared_ptr<std::string> value) { this->refresh_token = value; }

        std::shared_ptr<std::string> get_silo() const { return silo; }
        void set_silo(std::shared_ptr<std::string> value) { this->silo = value; }

        std::shared_ptr<std::string> get_user_id() const { return user_id; }
        void set_user_id(std::shared_ptr<std::string> value) { this->user_id = value; }

        std::shared_ptr<std::string> get_user_name() const { return user_name; }
        void set_user_name(std::shared_ptr<std::string> value) { this->user_name = value; }

        std::shared_ptr<std::string> get_user_shortname() const { return user_shortname; }
        void set_user_shortname(std::shared_ptr<std::string> value) { this->user_shortname = value; }

        std::shared_ptr<std::string> get_user_type() const { return user_type; }
        void set_user_type(std::shared_ptr<std::string> value) { this->user_type = value; }
    };

    class GenerateSession {
        public:
        GenerateSession() = default;
        virtual ~GenerateSession() = default;

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
    void from_json(const json & j, GenerateSession::Meta & x);
    void to_json(json & j, const GenerateSession::Meta & x);

    void from_json(const json & j, GenerateSession::Data & x);
    void to_json(json & j, const GenerateSession::Data & x);

    void from_json(const json & j, GenerateSession::GenerateSession & x);
    void to_json(json & j, const GenerateSession::GenerateSession & x);

    inline void from_json(const json & j, GenerateSession::Meta& x) {
        x.set_demat_consent(GenerateSession::get_optional<std::string>(j, "demat_consent"));
    }

    inline void to_json(json & j, const GenerateSession::Meta & x) {
        j = json::object();
        j["demat_consent"] = x.get_demat_consent();
    }

    inline void from_json(const json & j, GenerateSession::Data& x) {
        x.set_access_token(GenerateSession::get_optional<std::string>(j, "access_token"));
        x.set_api_key(GenerateSession::get_optional<std::string>(j, "api_key"));
        x.set_avatar_url(GenerateSession::get_optional<std::string>(j, "avatar_url"));
        x.set_broker(GenerateSession::get_optional<std::string>(j, "broker"));
        x.set_email(GenerateSession::get_optional<std::string>(j, "email"));
        x.set_enctoken(GenerateSession::get_optional<std::string>(j, "enctoken"));
        x.set_exchanges(GenerateSession::get_optional<std::vector<std::string>>(j, "exchanges"));
        x.set_login_time(GenerateSession::get_optional<std::string>(j, "login_time"));
        x.set_meta(GenerateSession::get_optional<GenerateSession::Meta>(j, "meta"));
        x.set_order_types(GenerateSession::get_optional<std::vector<std::string>>(j, "order_types"));
        x.set_products(GenerateSession::get_optional<std::vector<std::string>>(j, "products"));
        x.set_public_token(GenerateSession::get_optional<std::string>(j, "public_token"));
        x.set_refresh_token(GenerateSession::get_optional<std::string>(j, "refresh_token"));
        x.set_silo(GenerateSession::get_optional<std::string>(j, "silo"));
        x.set_user_id(GenerateSession::get_optional<std::string>(j, "user_id"));
        x.set_user_name(GenerateSession::get_optional<std::string>(j, "user_name"));
        x.set_user_shortname(GenerateSession::get_optional<std::string>(j, "user_shortname"));
        x.set_user_type(GenerateSession::get_optional<std::string>(j, "user_type"));
    }

    inline void to_json(json & j, const GenerateSession::Data & x) {
        j = json::object();
        j["access_token"] = x.get_access_token();
        j["api_key"] = x.get_api_key();
        j["avatar_url"] = x.get_avatar_url();
        j["broker"] = x.get_broker();
        j["email"] = x.get_email();
        j["enctoken"] = x.get_enctoken();
        j["exchanges"] = x.get_exchanges();
        j["login_time"] = x.get_login_time();
        j["meta"] = x.get_meta();
        j["order_types"] = x.get_order_types();
        j["products"] = x.get_products();
        j["public_token"] = x.get_public_token();
        j["refresh_token"] = x.get_refresh_token();
        j["silo"] = x.get_silo();
        j["user_id"] = x.get_user_id();
        j["user_name"] = x.get_user_name();
        j["user_shortname"] = x.get_user_shortname();
        j["user_type"] = x.get_user_type();
    }

    inline void from_json(const json & j, GenerateSession::GenerateSession& x) {
        x.set_data(GenerateSession::get_optional<GenerateSession::Data>(j, "data"));
        x.set_status(GenerateSession::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const GenerateSession::GenerateSession & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
