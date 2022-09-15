//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     MarginsEquity data = nlohmann::json::parse(jsonString);

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

    class AdhocMargin {
        public:
        AdhocMargin() = default;
        virtual ~AdhocMargin() = default;

        private:
        std::string type;

        public:
        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class AvailableProperties {
        public:
        AvailableProperties() = default;
        virtual ~AvailableProperties() = default;

        private:
        AdhocMargin adhoc_margin;
        AdhocMargin cash;
        AdhocMargin collateral;
        AdhocMargin intraday_payin;
        AdhocMargin live_balance;
        AdhocMargin opening_balance;

        public:
        const AdhocMargin & get_adhoc_margin() const { return adhoc_margin; }
        AdhocMargin & get_mutable_adhoc_margin() { return adhoc_margin; }
        void set_adhoc_margin(const AdhocMargin & value) { this->adhoc_margin = value; }

        const AdhocMargin & get_cash() const { return cash; }
        AdhocMargin & get_mutable_cash() { return cash; }
        void set_cash(const AdhocMargin & value) { this->cash = value; }

        const AdhocMargin & get_collateral() const { return collateral; }
        AdhocMargin & get_mutable_collateral() { return collateral; }
        void set_collateral(const AdhocMargin & value) { this->collateral = value; }

        const AdhocMargin & get_intraday_payin() const { return intraday_payin; }
        AdhocMargin & get_mutable_intraday_payin() { return intraday_payin; }
        void set_intraday_payin(const AdhocMargin & value) { this->intraday_payin = value; }

        const AdhocMargin & get_live_balance() const { return live_balance; }
        AdhocMargin & get_mutable_live_balance() { return live_balance; }
        void set_live_balance(const AdhocMargin & value) { this->live_balance = value; }

        const AdhocMargin & get_opening_balance() const { return opening_balance; }
        AdhocMargin & get_mutable_opening_balance() { return opening_balance; }
        void set_opening_balance(const AdhocMargin & value) { this->opening_balance = value; }
    };

    class Available {
        public:
        Available() = default;
        virtual ~Available() = default;

        private:
        bool additional_properties;
        AvailableProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const AvailableProperties & get_properties() const { return properties; }
        AvailableProperties & get_mutable_properties() { return properties; }
        void set_properties(const AvailableProperties & value) { this->properties = value; }

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

    class AvailableClass {
        public:
        AvailableClass() = default;
        virtual ~AvailableClass() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    class Utilised {
        public:
        Utilised() = default;
        virtual ~Utilised() = default;

        private:
        AdhocMargin additional_properties;
        std::string type;

        public:
        const AdhocMargin & get_additional_properties() const { return additional_properties; }
        AdhocMargin & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const AdhocMargin & value) { this->additional_properties = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class DataProperties {
        public:
        DataProperties() = default;
        virtual ~DataProperties() = default;

        private:
        AvailableClass available;
        AdhocMargin enabled;
        AdhocMargin net;
        Utilised utilised;

        public:
        const AvailableClass & get_available() const { return available; }
        AvailableClass & get_mutable_available() { return available; }
        void set_available(const AvailableClass & value) { this->available = value; }

        const AdhocMargin & get_enabled() const { return enabled; }
        AdhocMargin & get_mutable_enabled() { return enabled; }
        void set_enabled(const AdhocMargin & value) { this->enabled = value; }

        const AdhocMargin & get_net() const { return net; }
        AdhocMargin & get_mutable_net() { return net; }
        void set_net(const AdhocMargin & value) { this->net = value; }

        const Utilised & get_utilised() const { return utilised; }
        Utilised & get_mutable_utilised() { return utilised; }
        void set_utilised(const Utilised & value) { this->utilised = value; }
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

    class MarginsEquityProperties {
        public:
        MarginsEquityProperties() = default;
        virtual ~MarginsEquityProperties() = default;

        private:
        AvailableClass data;
        AdhocMargin status;

        public:
        const AvailableClass & get_data() const { return data; }
        AvailableClass & get_mutable_data() { return data; }
        void set_data(const AvailableClass & value) { this->data = value; }

        const AdhocMargin & get_status() const { return status; }
        AdhocMargin & get_mutable_status() { return status; }
        void set_status(const AdhocMargin & value) { this->status = value; }
    };

    class MarginsEquityClass {
        public:
        MarginsEquityClass() = default;
        virtual ~MarginsEquityClass() = default;

        private:
        bool additional_properties;
        MarginsEquityProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const MarginsEquityProperties & get_properties() const { return properties; }
        MarginsEquityProperties & get_mutable_properties() { return properties; }
        void set_properties(const MarginsEquityProperties & value) { this->properties = value; }

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
        Available available;
        Data data;
        MarginsEquityClass margins_equity;

        public:
        const Available & get_available() const { return available; }
        Available & get_mutable_available() { return available; }
        void set_available(const Available & value) { this->available = value; }

        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const MarginsEquityClass & get_margins_equity() const { return margins_equity; }
        MarginsEquityClass & get_mutable_margins_equity() { return margins_equity; }
        void set_margins_equity(const MarginsEquityClass & value) { this->margins_equity = value; }
    };

    class MarginsEquity {
        public:
        MarginsEquity() = default;
        virtual ~MarginsEquity() = default;

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
    void from_json(const json & j, quicktype::AdhocMargin & x);
    void to_json(json & j, const quicktype::AdhocMargin & x);

    void from_json(const json & j, quicktype::AvailableProperties & x);
    void to_json(json & j, const quicktype::AvailableProperties & x);

    void from_json(const json & j, quicktype::Available & x);
    void to_json(json & j, const quicktype::Available & x);

    void from_json(const json & j, quicktype::AvailableClass & x);
    void to_json(json & j, const quicktype::AvailableClass & x);

    void from_json(const json & j, quicktype::Utilised & x);
    void to_json(json & j, const quicktype::Utilised & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::MarginsEquityProperties & x);
    void to_json(json & j, const quicktype::MarginsEquityProperties & x);

    void from_json(const json & j, quicktype::MarginsEquityClass & x);
    void to_json(json & j, const quicktype::MarginsEquityClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::MarginsEquity & x);
    void to_json(json & j, const quicktype::MarginsEquity & x);

    inline void from_json(const json & j, quicktype::AdhocMargin& x) {
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::AdhocMargin & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::AvailableProperties& x) {
        x.set_adhoc_margin(j.at("adhoc_margin").get<quicktype::AdhocMargin>());
        x.set_cash(j.at("cash").get<quicktype::AdhocMargin>());
        x.set_collateral(j.at("collateral").get<quicktype::AdhocMargin>());
        x.set_intraday_payin(j.at("intraday_payin").get<quicktype::AdhocMargin>());
        x.set_live_balance(j.at("live_balance").get<quicktype::AdhocMargin>());
        x.set_opening_balance(j.at("opening_balance").get<quicktype::AdhocMargin>());
    }

    inline void to_json(json & j, const quicktype::AvailableProperties & x) {
        j = json::object();
        j["adhoc_margin"] = x.get_adhoc_margin();
        j["cash"] = x.get_cash();
        j["collateral"] = x.get_collateral();
        j["intraday_payin"] = x.get_intraday_payin();
        j["live_balance"] = x.get_live_balance();
        j["opening_balance"] = x.get_opening_balance();
    }

    inline void from_json(const json & j, quicktype::Available& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::AvailableProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Available & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::AvailableClass& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::AvailableClass & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::Utilised& x) {
        x.set_additional_properties(j.at("additionalProperties").get<quicktype::AdhocMargin>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Utilised & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_available(j.at("available").get<quicktype::AvailableClass>());
        x.set_enabled(j.at("enabled").get<quicktype::AdhocMargin>());
        x.set_net(j.at("net").get<quicktype::AdhocMargin>());
        x.set_utilised(j.at("utilised").get<quicktype::Utilised>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["available"] = x.get_available();
        j["enabled"] = x.get_enabled();
        j["net"] = x.get_net();
        j["utilised"] = x.get_utilised();
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

    inline void from_json(const json & j, quicktype::MarginsEquityProperties& x) {
        x.set_data(j.at("data").get<quicktype::AvailableClass>());
        x.set_status(j.at("status").get<quicktype::AdhocMargin>());
    }

    inline void to_json(json & j, const quicktype::MarginsEquityProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::MarginsEquityClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::MarginsEquityProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MarginsEquityClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_available(j.at("Available").get<quicktype::Available>());
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_margins_equity(j.at("MarginsEquity").get<quicktype::MarginsEquityClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Available"] = x.get_available();
        j["Data"] = x.get_data();
        j["MarginsEquity"] = x.get_margins_equity();
    }

    inline void from_json(const json & j, quicktype::MarginsEquity& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::MarginsEquity & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }
}
