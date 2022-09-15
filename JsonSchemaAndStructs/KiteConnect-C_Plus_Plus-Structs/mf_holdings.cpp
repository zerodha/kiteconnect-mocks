//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     MfHoldings data = nlohmann::json::parse(jsonString);

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

    enum class Type : int { INTEGER, NUMBER, STRING };

    class AveragePrice {
        public:
        AveragePrice() = default;
        virtual ~AveragePrice() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class DatumProperties {
        public:
        DatumProperties() = default;
        virtual ~DatumProperties() = default;

        private:
        AveragePrice average_price;
        AveragePrice folio;
        AveragePrice fund;
        AveragePrice last_price;
        AveragePrice last_price_date;
        AveragePrice pledged_quantity;
        AveragePrice pnl;
        AveragePrice quantity;
        AveragePrice tradingsymbol;

        public:
        const AveragePrice & get_average_price() const { return average_price; }
        AveragePrice & get_mutable_average_price() { return average_price; }
        void set_average_price(const AveragePrice & value) { this->average_price = value; }

        const AveragePrice & get_folio() const { return folio; }
        AveragePrice & get_mutable_folio() { return folio; }
        void set_folio(const AveragePrice & value) { this->folio = value; }

        const AveragePrice & get_fund() const { return fund; }
        AveragePrice & get_mutable_fund() { return fund; }
        void set_fund(const AveragePrice & value) { this->fund = value; }

        const AveragePrice & get_last_price() const { return last_price; }
        AveragePrice & get_mutable_last_price() { return last_price; }
        void set_last_price(const AveragePrice & value) { this->last_price = value; }

        const AveragePrice & get_last_price_date() const { return last_price_date; }
        AveragePrice & get_mutable_last_price_date() { return last_price_date; }
        void set_last_price_date(const AveragePrice & value) { this->last_price_date = value; }

        const AveragePrice & get_pledged_quantity() const { return pledged_quantity; }
        AveragePrice & get_mutable_pledged_quantity() { return pledged_quantity; }
        void set_pledged_quantity(const AveragePrice & value) { this->pledged_quantity = value; }

        const AveragePrice & get_pnl() const { return pnl; }
        AveragePrice & get_mutable_pnl() { return pnl; }
        void set_pnl(const AveragePrice & value) { this->pnl = value; }

        const AveragePrice & get_quantity() const { return quantity; }
        AveragePrice & get_mutable_quantity() { return quantity; }
        void set_quantity(const AveragePrice & value) { this->quantity = value; }

        const AveragePrice & get_tradingsymbol() const { return tradingsymbol; }
        AveragePrice & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const AveragePrice & value) { this->tradingsymbol = value; }
    };

    class Datum {
        public:
        Datum() = default;
        virtual ~Datum() = default;

        private:
        bool additional_properties;
        DatumProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const DatumProperties & get_properties() const { return properties; }
        DatumProperties & get_mutable_properties() { return properties; }
        void set_properties(const DatumProperties & value) { this->properties = value; }

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

    class Items {
        public:
        Items() = default;
        virtual ~Items() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

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

    class MfHoldingsProperties {
        public:
        MfHoldingsProperties() = default;
        virtual ~MfHoldingsProperties() = default;

        private:
        Data data;
        AveragePrice status;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const AveragePrice & get_status() const { return status; }
        AveragePrice & get_mutable_status() { return status; }
        void set_status(const AveragePrice & value) { this->status = value; }
    };

    class MfHoldingsClass {
        public:
        MfHoldingsClass() = default;
        virtual ~MfHoldingsClass() = default;

        private:
        bool additional_properties;
        MfHoldingsProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const MfHoldingsProperties & get_properties() const { return properties; }
        MfHoldingsProperties & get_mutable_properties() { return properties; }
        void set_properties(const MfHoldingsProperties & value) { this->properties = value; }

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
        Datum datum;
        MfHoldingsClass mf_holdings;

        public:
        const Datum & get_datum() const { return datum; }
        Datum & get_mutable_datum() { return datum; }
        void set_datum(const Datum & value) { this->datum = value; }

        const MfHoldingsClass & get_mf_holdings() const { return mf_holdings; }
        MfHoldingsClass & get_mutable_mf_holdings() { return mf_holdings; }
        void set_mf_holdings(const MfHoldingsClass & value) { this->mf_holdings = value; }
    };

    class MfHoldings {
        public:
        MfHoldings() = default;
        virtual ~MfHoldings() = default;

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
    void from_json(const json & j, quicktype::AveragePrice & x);
    void to_json(json & j, const quicktype::AveragePrice & x);

    void from_json(const json & j, quicktype::DatumProperties & x);
    void to_json(json & j, const quicktype::DatumProperties & x);

    void from_json(const json & j, quicktype::Datum & x);
    void to_json(json & j, const quicktype::Datum & x);

    void from_json(const json & j, quicktype::Items & x);
    void to_json(json & j, const quicktype::Items & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::MfHoldingsProperties & x);
    void to_json(json & j, const quicktype::MfHoldingsProperties & x);

    void from_json(const json & j, quicktype::MfHoldingsClass & x);
    void to_json(json & j, const quicktype::MfHoldingsClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::MfHoldings & x);
    void to_json(json & j, const quicktype::MfHoldings & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::AveragePrice& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::AveragePrice & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DatumProperties& x) {
        x.set_average_price(j.at("average_price").get<quicktype::AveragePrice>());
        x.set_folio(j.at("folio").get<quicktype::AveragePrice>());
        x.set_fund(j.at("fund").get<quicktype::AveragePrice>());
        x.set_last_price(j.at("last_price").get<quicktype::AveragePrice>());
        x.set_last_price_date(j.at("last_price_date").get<quicktype::AveragePrice>());
        x.set_pledged_quantity(j.at("pledged_quantity").get<quicktype::AveragePrice>());
        x.set_pnl(j.at("pnl").get<quicktype::AveragePrice>());
        x.set_quantity(j.at("quantity").get<quicktype::AveragePrice>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::DatumProperties & x) {
        j = json::object();
        j["average_price"] = x.get_average_price();
        j["folio"] = x.get_folio();
        j["fund"] = x.get_fund();
        j["last_price"] = x.get_last_price();
        j["last_price_date"] = x.get_last_price_date();
        j["pledged_quantity"] = x.get_pledged_quantity();
        j["pnl"] = x.get_pnl();
        j["quantity"] = x.get_quantity();
        j["tradingsymbol"] = x.get_tradingsymbol();
    }

    inline void from_json(const json & j, quicktype::Datum& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::DatumProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Datum & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Items& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Items & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::Data& x) {
        x.set_items(j.at("items").get<quicktype::Items>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Data & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::MfHoldingsProperties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
        x.set_status(j.at("status").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::MfHoldingsProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::MfHoldingsClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::MfHoldingsProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MfHoldingsClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_datum(j.at("Datum").get<quicktype::Datum>());
        x.set_mf_holdings(j.at("MFHoldings").get<quicktype::MfHoldingsClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Datum"] = x.get_datum();
        j["MFHoldings"] = x.get_mf_holdings();
    }

    inline void from_json(const json & j, quicktype::MfHoldings& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::MfHoldings & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }

    inline void from_json(const json & j, quicktype::Type & x) {
        if (j == "integer") x = quicktype::Type::INTEGER;
        else if (j == "number") x = quicktype::Type::NUMBER;
        else if (j == "string") x = quicktype::Type::STRING;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const quicktype::Type & x) {
        switch (x) {
            case quicktype::Type::INTEGER: j = "integer"; break;
            case quicktype::Type::NUMBER: j = "number"; break;
            case quicktype::Type::STRING: j = "string"; break;
            default: throw "This should not happen";
        }
    }
}
