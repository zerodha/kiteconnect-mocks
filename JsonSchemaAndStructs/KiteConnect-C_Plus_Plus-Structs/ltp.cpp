//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Ltp data = nlohmann::json::parse(jsonString);

#pragma once

#include "json.hpp"

#include <boost/optional.hpp>
#include <stdexcept>
#include <regex>

namespace quicktype {
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

    class NseInfy {
        public:
        NseInfy() = default;
        virtual ~NseInfy() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    class DataProperties {
        public:
        DataProperties() = default;
        virtual ~DataProperties() = default;

        private:
        NseInfy nse_infy;

        public:
        const NseInfy & get_nse_infy() const { return nse_infy; }
        NseInfy & get_mutable_nse_infy() { return nse_infy; }
        void set_nse_infy(const NseInfy & value) { this->nse_infy = value; }
    };

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        bool additional_properties;
        DataProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const DataProperties & get_properties() const { return properties; }
        DataProperties & get_mutable_properties() { return properties; }
        void set_properties(const DataProperties & value) { this->properties = value; }

        const std::vector<std::string> & get_required() const { return required; }
        std::vector<std::string> & get_mutable_required() { return required; }
        void set_required(const std::vector<std::string> & value) { this->required = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class Status {
        public:
        Status() = default;
        virtual ~Status() = default;

        private:
        std::string type;

        public:
        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class LtpProperties {
        public:
        LtpProperties() = default;
        virtual ~LtpProperties() = default;

        private:
        NseInfy data;
        Status status;

        public:
        const NseInfy & get_data() const { return data; }
        NseInfy & get_mutable_data() { return data; }
        void set_data(const NseInfy & value) { this->data = value; }

        const Status & get_status() const { return status; }
        Status & get_mutable_status() { return status; }
        void set_status(const Status & value) { this->status = value; }
    };

    class LtpClass {
        public:
        LtpClass() = default;
        virtual ~LtpClass() = default;

        private:
        bool additional_properties;
        LtpProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const LtpProperties & get_properties() const { return properties; }
        LtpProperties & get_mutable_properties() { return properties; }
        void set_properties(const LtpProperties & value) { this->properties = value; }

        const std::vector<std::string> & get_required() const { return required; }
        std::vector<std::string> & get_mutable_required() { return required; }
        void set_required(const std::vector<std::string> & value) { this->required = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class NseInfyProperties {
        public:
        NseInfyProperties() = default;
        virtual ~NseInfyProperties() = default;

        private:
        Status instrument_token;
        Status last_price;

        public:
        const Status & get_instrument_token() const { return instrument_token; }
        Status & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const Status & value) { this->instrument_token = value; }

        const Status & get_last_price() const { return last_price; }
        Status & get_mutable_last_price() { return last_price; }
        void set_last_price(const Status & value) { this->last_price = value; }
    };

    class NseInfyClass {
        public:
        NseInfyClass() = default;
        virtual ~NseInfyClass() = default;

        private:
        bool additional_properties;
        NseInfyProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const NseInfyProperties & get_properties() const { return properties; }
        NseInfyProperties & get_mutable_properties() { return properties; }
        void set_properties(const NseInfyProperties & value) { this->properties = value; }

        const std::vector<std::string> & get_required() const { return required; }
        std::vector<std::string> & get_mutable_required() { return required; }
        void set_required(const std::vector<std::string> & value) { this->required = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class Definitions {
        public:
        Definitions() = default;
        virtual ~Definitions() = default;

        private:
        Data data;
        LtpClass ltp;
        NseInfyClass nse_infy;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const LtpClass & get_ltp() const { return ltp; }
        LtpClass & get_mutable_ltp() { return ltp; }
        void set_ltp(const LtpClass & value) { this->ltp = value; }

        const NseInfyClass & get_nse_infy() const { return nse_infy; }
        NseInfyClass & get_mutable_nse_infy() { return nse_infy; }
        void set_nse_infy(const NseInfyClass & value) { this->nse_infy = value; }
    };

    class Ltp {
        public:
        Ltp() = default;
        virtual ~Ltp() = default;

        private:
        std::string ref;
        std::string schema;
        Definitions definitions;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }

        const std::string & get_schema() const { return schema; }
        std::string & get_mutable_schema() { return schema; }
        void set_schema(const std::string & value) { this->schema = value; }

        const Definitions & get_definitions() const { return definitions; }
        Definitions & get_mutable_definitions() { return definitions; }
        void set_definitions(const Definitions & value) { this->definitions = value; }
    };
}

namespace nlohmann {
    void from_json(const json & j, quicktype::NseInfy & x);
    void to_json(json & j, const quicktype::NseInfy & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::Status & x);
    void to_json(json & j, const quicktype::Status & x);

    void from_json(const json & j, quicktype::LtpProperties & x);
    void to_json(json & j, const quicktype::LtpProperties & x);

    void from_json(const json & j, quicktype::LtpClass & x);
    void to_json(json & j, const quicktype::LtpClass & x);

    void from_json(const json & j, quicktype::NseInfyProperties & x);
    void to_json(json & j, const quicktype::NseInfyProperties & x);

    void from_json(const json & j, quicktype::NseInfyClass & x);
    void to_json(json & j, const quicktype::NseInfyClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::Ltp & x);
    void to_json(json & j, const quicktype::Ltp & x);

    inline void from_json(const json & j, quicktype::NseInfy& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::NseInfy & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_nse_infy(j.at("NSE:INFY").get<quicktype::NseInfy>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["NSE:INFY"] = x.get_nse_infy();
    }

    inline void from_json(const json & j, quicktype::Data& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::DataProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Data & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Status& x) {
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Status & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::LtpProperties& x) {
        x.set_data(j.at("data").get<quicktype::NseInfy>());
        x.set_status(j.at("status").get<quicktype::Status>());
    }

    inline void to_json(json & j, const quicktype::LtpProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::LtpClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::LtpProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::LtpClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::NseInfyProperties& x) {
        x.set_instrument_token(j.at("instrument_token").get<quicktype::Status>());
        x.set_last_price(j.at("last_price").get<quicktype::Status>());
    }

    inline void to_json(json & j, const quicktype::NseInfyProperties & x) {
        j = json::object();
        j["instrument_token"] = x.get_instrument_token();
        j["last_price"] = x.get_last_price();
    }

    inline void from_json(const json & j, quicktype::NseInfyClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::NseInfyProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::NseInfyClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_ltp(j.at("Ltp").get<quicktype::LtpClass>());
        x.set_nse_infy(j.at("NseInfy").get<quicktype::NseInfyClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Data"] = x.get_data();
        j["Ltp"] = x.get_ltp();
        j["NseInfy"] = x.get_nse_infy();
    }

    inline void from_json(const json & j, quicktype::Ltp& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::Ltp & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }
}
