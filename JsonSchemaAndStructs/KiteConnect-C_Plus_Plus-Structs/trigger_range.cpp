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
        NseInfy nse_reliance;

        public:
        const NseInfy & get_nse_infy() const { return nse_infy; }
        NseInfy & get_mutable_nse_infy() { return nse_infy; }
        void set_nse_infy(const NseInfy & value) { this->nse_infy = value; }

        const NseInfy & get_nse_reliance() const { return nse_reliance; }
        NseInfy & get_mutable_nse_reliance() { return nse_reliance; }
        void set_nse_reliance(const NseInfy & value) { this->nse_reliance = value; }
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

    class InstrumentToken {
        public:
        InstrumentToken() = default;
        virtual ~InstrumentToken() = default;

        private:
        std::string type;

        public:
        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class NseProperties {
        public:
        NseProperties() = default;
        virtual ~NseProperties() = default;

        private:
        InstrumentToken instrument_token;
        InstrumentToken lower;
        InstrumentToken upper;

        public:
        const InstrumentToken & get_instrument_token() const { return instrument_token; }
        InstrumentToken & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const InstrumentToken & value) { this->instrument_token = value; }

        const InstrumentToken & get_lower() const { return lower; }
        InstrumentToken & get_mutable_lower() { return lower; }
        void set_lower(const InstrumentToken & value) { this->lower = value; }

        const InstrumentToken & get_upper() const { return upper; }
        InstrumentToken & get_mutable_upper() { return upper; }
        void set_upper(const InstrumentToken & value) { this->upper = value; }
    };

    class Nse {
        public:
        Nse() = default;
        virtual ~Nse() = default;

        private:
        bool additional_properties;
        NseProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const NseProperties & get_properties() const { return properties; }
        NseProperties & get_mutable_properties() { return properties; }
        void set_properties(const NseProperties & value) { this->properties = value; }

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

    class TriggerRangeProperties {
        public:
        TriggerRangeProperties() = default;
        virtual ~TriggerRangeProperties() = default;

        private:
        NseInfy data;
        InstrumentToken status;

        public:
        const NseInfy & get_data() const { return data; }
        NseInfy & get_mutable_data() { return data; }
        void set_data(const NseInfy & value) { this->data = value; }

        const InstrumentToken & get_status() const { return status; }
        InstrumentToken & get_mutable_status() { return status; }
        void set_status(const InstrumentToken & value) { this->status = value; }
    };

    class TriggerRangeClass {
        public:
        TriggerRangeClass() = default;
        virtual ~TriggerRangeClass() = default;

        private:
        bool additional_properties;
        TriggerRangeProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const TriggerRangeProperties & get_properties() const { return properties; }
        TriggerRangeProperties & get_mutable_properties() { return properties; }
        void set_properties(const TriggerRangeProperties & value) { this->properties = value; }

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
        Nse nse;
        TriggerRangeClass trigger_range;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const Nse & get_nse() const { return nse; }
        Nse & get_mutable_nse() { return nse; }
        void set_nse(const Nse & value) { this->nse = value; }

        const TriggerRangeClass & get_trigger_range() const { return trigger_range; }
        TriggerRangeClass & get_mutable_trigger_range() { return trigger_range; }
        void set_trigger_range(const TriggerRangeClass & value) { this->trigger_range = value; }
    };

    class TriggerRange {
        public:
        TriggerRange() = default;
        virtual ~TriggerRange() = default;

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

    void from_json(const json & j, quicktype::InstrumentToken & x);
    void to_json(json & j, const quicktype::InstrumentToken & x);

    void from_json(const json & j, quicktype::NseProperties & x);
    void to_json(json & j, const quicktype::NseProperties & x);

    void from_json(const json & j, quicktype::Nse & x);
    void to_json(json & j, const quicktype::Nse & x);

    void from_json(const json & j, quicktype::TriggerRangeProperties & x);
    void to_json(json & j, const quicktype::TriggerRangeProperties & x);

    void from_json(const json & j, quicktype::TriggerRangeClass & x);
    void to_json(json & j, const quicktype::TriggerRangeClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::TriggerRange & x);
    void to_json(json & j, const quicktype::TriggerRange & x);

    inline void from_json(const json & j, quicktype::NseInfy& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::NseInfy & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_nse_infy(j.at("NSE:INFY").get<quicktype::NseInfy>());
        x.set_nse_reliance(j.at("NSE:RELIANCE").get<quicktype::NseInfy>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["NSE:INFY"] = x.get_nse_infy();
        j["NSE:RELIANCE"] = x.get_nse_reliance();
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

    inline void from_json(const json & j, quicktype::InstrumentToken& x) {
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::InstrumentToken & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::NseProperties& x) {
        x.set_instrument_token(j.at("instrument_token").get<quicktype::InstrumentToken>());
        x.set_lower(j.at("lower").get<quicktype::InstrumentToken>());
        x.set_upper(j.at("upper").get<quicktype::InstrumentToken>());
    }

    inline void to_json(json & j, const quicktype::NseProperties & x) {
        j = json::object();
        j["instrument_token"] = x.get_instrument_token();
        j["lower"] = x.get_lower();
        j["upper"] = x.get_upper();
    }

    inline void from_json(const json & j, quicktype::Nse& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::NseProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Nse & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::TriggerRangeProperties& x) {
        x.set_data(j.at("data").get<quicktype::NseInfy>());
        x.set_status(j.at("status").get<quicktype::InstrumentToken>());
    }

    inline void to_json(json & j, const quicktype::TriggerRangeProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::TriggerRangeClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::TriggerRangeProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::TriggerRangeClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_nse(j.at("Nse").get<quicktype::Nse>());
        x.set_trigger_range(j.at("TriggerRange").get<quicktype::TriggerRangeClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Data"] = x.get_data();
        j["Nse"] = x.get_nse();
        j["TriggerRange"] = x.get_trigger_range();
    }

    inline void from_json(const json & j, quicktype::TriggerRange& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::TriggerRange & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }
}
