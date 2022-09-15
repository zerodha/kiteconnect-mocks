//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     ConvertPosition data = nlohmann::json::parse(jsonString);

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

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        std::string type;

        public:
        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class Properties {
        public:
        Properties() = default;
        virtual ~Properties() = default;

        private:
        Data data;
        Data status;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const Data & get_status() const { return status; }
        Data & get_mutable_status() { return status; }
        void set_status(const Data & value) { this->status = value; }
    };

    class ConvertPositionClass {
        public:
        ConvertPositionClass() = default;
        virtual ~ConvertPositionClass() = default;

        private:
        bool additional_properties;
        Properties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const Properties & get_properties() const { return properties; }
        Properties & get_mutable_properties() { return properties; }
        void set_properties(const Properties & value) { this->properties = value; }

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
        ConvertPositionClass convert_position;

        public:
        const ConvertPositionClass & get_convert_position() const { return convert_position; }
        ConvertPositionClass & get_mutable_convert_position() { return convert_position; }
        void set_convert_position(const ConvertPositionClass & value) { this->convert_position = value; }
    };

    class ConvertPosition {
        public:
        ConvertPosition() = default;
        virtual ~ConvertPosition() = default;

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
    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::Properties & x);
    void to_json(json & j, const quicktype::Properties & x);

    void from_json(const json & j, quicktype::ConvertPositionClass & x);
    void to_json(json & j, const quicktype::ConvertPositionClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::ConvertPosition & x);
    void to_json(json & j, const quicktype::ConvertPosition & x);

    inline void from_json(const json & j, quicktype::Data& x) {
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Data & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Properties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
        x.set_status(j.at("status").get<quicktype::Data>());
    }

    inline void to_json(json & j, const quicktype::Properties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::ConvertPositionClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::Properties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::ConvertPositionClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_convert_position(j.at("ConvertPosition").get<quicktype::ConvertPositionClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["ConvertPosition"] = x.get_convert_position();
    }

    inline void from_json(const json & j, quicktype::ConvertPosition& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::ConvertPosition & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }
}
