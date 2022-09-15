//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     GttPlaceOrder data = nlohmann::json::parse(jsonString);

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

    class TriggerId {
        public:
        TriggerId() = default;
        virtual ~TriggerId() = default;

        private:
        std::string type;

        public:
        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class DataProperties {
        public:
        DataProperties() = default;
        virtual ~DataProperties() = default;

        private:
        TriggerId trigger_id;

        public:
        const TriggerId & get_trigger_id() const { return trigger_id; }
        TriggerId & get_mutable_trigger_id() { return trigger_id; }
        void set_trigger_id(const TriggerId & value) { this->trigger_id = value; }
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

    class DataClass {
        public:
        DataClass() = default;
        virtual ~DataClass() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    class GttPlaceOrderProperties {
        public:
        GttPlaceOrderProperties() = default;
        virtual ~GttPlaceOrderProperties() = default;

        private:
        DataClass data;
        TriggerId status;

        public:
        const DataClass & get_data() const { return data; }
        DataClass & get_mutable_data() { return data; }
        void set_data(const DataClass & value) { this->data = value; }

        const TriggerId & get_status() const { return status; }
        TriggerId & get_mutable_status() { return status; }
        void set_status(const TriggerId & value) { this->status = value; }
    };

    class GttPlaceOrderClass {
        public:
        GttPlaceOrderClass() = default;
        virtual ~GttPlaceOrderClass() = default;

        private:
        bool additional_properties;
        GttPlaceOrderProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const GttPlaceOrderProperties & get_properties() const { return properties; }
        GttPlaceOrderProperties & get_mutable_properties() { return properties; }
        void set_properties(const GttPlaceOrderProperties & value) { this->properties = value; }

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
        GttPlaceOrderClass gtt_place_order;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const GttPlaceOrderClass & get_gtt_place_order() const { return gtt_place_order; }
        GttPlaceOrderClass & get_mutable_gtt_place_order() { return gtt_place_order; }
        void set_gtt_place_order(const GttPlaceOrderClass & value) { this->gtt_place_order = value; }
    };

    class GttPlaceOrder {
        public:
        GttPlaceOrder() = default;
        virtual ~GttPlaceOrder() = default;

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
    void from_json(const json & j, quicktype::TriggerId & x);
    void to_json(json & j, const quicktype::TriggerId & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::DataClass & x);
    void to_json(json & j, const quicktype::DataClass & x);

    void from_json(const json & j, quicktype::GttPlaceOrderProperties & x);
    void to_json(json & j, const quicktype::GttPlaceOrderProperties & x);

    void from_json(const json & j, quicktype::GttPlaceOrderClass & x);
    void to_json(json & j, const quicktype::GttPlaceOrderClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::GttPlaceOrder & x);
    void to_json(json & j, const quicktype::GttPlaceOrder & x);

    inline void from_json(const json & j, quicktype::TriggerId& x) {
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::TriggerId & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_trigger_id(j.at("trigger_id").get<quicktype::TriggerId>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["trigger_id"] = x.get_trigger_id();
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

    inline void from_json(const json & j, quicktype::DataClass& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::DataClass & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::GttPlaceOrderProperties& x) {
        x.set_data(j.at("data").get<quicktype::DataClass>());
        x.set_status(j.at("status").get<quicktype::TriggerId>());
    }

    inline void to_json(json & j, const quicktype::GttPlaceOrderProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::GttPlaceOrderClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::GttPlaceOrderProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::GttPlaceOrderClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_gtt_place_order(j.at("GttPlaceOrder").get<quicktype::GttPlaceOrderClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Data"] = x.get_data();
        j["GttPlaceOrder"] = x.get_gtt_place_order();
    }

    inline void from_json(const json & j, quicktype::GttPlaceOrder& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::GttPlaceOrder & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }
}
