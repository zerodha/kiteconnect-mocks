//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     HistoricalOi data = nlohmann::json::parse(jsonString);

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

    class AnyOf {
        public:
        AnyOf() = default;
        virtual ~AnyOf() = default;

        private:
        std::string type;

        public:
        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class Candle {
        public:
        Candle() = default;
        virtual ~Candle() = default;

        private:
        std::vector<AnyOf> any_of;
        std::string title;

        public:
        const std::vector<AnyOf> & get_any_of() const { return any_of; }
        std::vector<AnyOf> & get_mutable_any_of() { return any_of; }
        void set_any_of(const std::vector<AnyOf> & value) { this->any_of = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }
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

    class Items {
        public:
        Items() = default;
        virtual ~Items() = default;

        private:
        DataClass items;
        std::string type;

        public:
        const DataClass & get_items() const { return items; }
        DataClass & get_mutable_items() { return items; }
        void set_items(const DataClass & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class Candles {
        public:
        Candles() = default;
        virtual ~Candles() = default;

        private:
        Items items;
        std::string type;

        public:
        const Items & get_items() const { return items; }
        Items & get_mutable_items() { return items; }
        void set_items(const Items & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class DataProperties {
        public:
        DataProperties() = default;
        virtual ~DataProperties() = default;

        private:
        Candles candles;

        public:
        const Candles & get_candles() const { return candles; }
        Candles & get_mutable_candles() { return candles; }
        void set_candles(const Candles & value) { this->candles = value; }
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

    class HistoricalOiProperties {
        public:
        HistoricalOiProperties() = default;
        virtual ~HistoricalOiProperties() = default;

        private:
        DataClass data;
        AnyOf status;

        public:
        const DataClass & get_data() const { return data; }
        DataClass & get_mutable_data() { return data; }
        void set_data(const DataClass & value) { this->data = value; }

        const AnyOf & get_status() const { return status; }
        AnyOf & get_mutable_status() { return status; }
        void set_status(const AnyOf & value) { this->status = value; }
    };

    class HistoricalOiClass {
        public:
        HistoricalOiClass() = default;
        virtual ~HistoricalOiClass() = default;

        private:
        bool additional_properties;
        HistoricalOiProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const HistoricalOiProperties & get_properties() const { return properties; }
        HistoricalOiProperties & get_mutable_properties() { return properties; }
        void set_properties(const HistoricalOiProperties & value) { this->properties = value; }

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
        Candle candle;
        Data data;
        HistoricalOiClass historical_oi;

        public:
        const Candle & get_candle() const { return candle; }
        Candle & get_mutable_candle() { return candle; }
        void set_candle(const Candle & value) { this->candle = value; }

        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const HistoricalOiClass & get_historical_oi() const { return historical_oi; }
        HistoricalOiClass & get_mutable_historical_oi() { return historical_oi; }
        void set_historical_oi(const HistoricalOiClass & value) { this->historical_oi = value; }
    };

    class HistoricalOi {
        public:
        HistoricalOi() = default;
        virtual ~HistoricalOi() = default;

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
    void from_json(const json & j, quicktype::AnyOf & x);
    void to_json(json & j, const quicktype::AnyOf & x);

    void from_json(const json & j, quicktype::Candle & x);
    void to_json(json & j, const quicktype::Candle & x);

    void from_json(const json & j, quicktype::DataClass & x);
    void to_json(json & j, const quicktype::DataClass & x);

    void from_json(const json & j, quicktype::Items & x);
    void to_json(json & j, const quicktype::Items & x);

    void from_json(const json & j, quicktype::Candles & x);
    void to_json(json & j, const quicktype::Candles & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::HistoricalOiProperties & x);
    void to_json(json & j, const quicktype::HistoricalOiProperties & x);

    void from_json(const json & j, quicktype::HistoricalOiClass & x);
    void to_json(json & j, const quicktype::HistoricalOiClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::HistoricalOi & x);
    void to_json(json & j, const quicktype::HistoricalOi & x);

    inline void from_json(const json & j, quicktype::AnyOf& x) {
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::AnyOf & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Candle& x) {
        x.set_any_of(j.at("anyOf").get<std::vector<quicktype::AnyOf>>());
        x.set_title(j.at("title").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Candle & x) {
        j = json::object();
        j["anyOf"] = x.get_any_of();
        j["title"] = x.get_title();
    }

    inline void from_json(const json & j, quicktype::DataClass& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::DataClass & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::Items& x) {
        x.set_items(j.at("items").get<quicktype::DataClass>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Items & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Candles& x) {
        x.set_items(j.at("items").get<quicktype::Items>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Candles & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_candles(j.at("candles").get<quicktype::Candles>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["candles"] = x.get_candles();
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

    inline void from_json(const json & j, quicktype::HistoricalOiProperties& x) {
        x.set_data(j.at("data").get<quicktype::DataClass>());
        x.set_status(j.at("status").get<quicktype::AnyOf>());
    }

    inline void to_json(json & j, const quicktype::HistoricalOiProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::HistoricalOiClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::HistoricalOiProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::HistoricalOiClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_candle(j.at("Candle").get<quicktype::Candle>());
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_historical_oi(j.at("HistoricalOi").get<quicktype::HistoricalOiClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Candle"] = x.get_candle();
        j["Data"] = x.get_data();
        j["HistoricalOi"] = x.get_historical_oi();
    }

    inline void from_json(const json & j, quicktype::HistoricalOi& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::HistoricalOi & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }
}
