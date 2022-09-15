//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Holdings data = nlohmann::json::parse(jsonString);

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

    enum class Type : int { BOOLEAN, INTEGER, NUMBER, STRING };

    class AuthorisedDate {
        public:
        AuthorisedDate() = default;
        virtual ~AuthorisedDate() = default;

        private:
        std::string format;
        Type type;

        public:
        const std::string & get_format() const { return format; }
        std::string & get_mutable_format() { return format; }
        void set_format(const std::string & value) { this->format = value; }

        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class AuthorisedQuantity {
        public:
        AuthorisedQuantity() = default;
        virtual ~AuthorisedQuantity() = default;

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
        AuthorisedDate authorised_date;
        AuthorisedQuantity authorised_quantity;
        AuthorisedQuantity average_price;
        AuthorisedQuantity close_price;
        AuthorisedQuantity collateral_quantity;
        AuthorisedQuantity collateral_type;
        AuthorisedQuantity day_change;
        AuthorisedQuantity day_change_percentage;
        AuthorisedQuantity discrepancy;
        AuthorisedQuantity exchange;
        AuthorisedQuantity instrument_token;
        AuthorisedQuantity isin;
        AuthorisedQuantity last_price;
        AuthorisedQuantity opening_quantity;
        AuthorisedQuantity pnl;
        AuthorisedQuantity price;
        AuthorisedQuantity product;
        AuthorisedQuantity quantity;
        AuthorisedQuantity realised_quantity;
        AuthorisedQuantity t1_quantity;
        AuthorisedQuantity tradingsymbol;
        AuthorisedQuantity used_quantity;

        public:
        const AuthorisedDate & get_authorised_date() const { return authorised_date; }
        AuthorisedDate & get_mutable_authorised_date() { return authorised_date; }
        void set_authorised_date(const AuthorisedDate & value) { this->authorised_date = value; }

        const AuthorisedQuantity & get_authorised_quantity() const { return authorised_quantity; }
        AuthorisedQuantity & get_mutable_authorised_quantity() { return authorised_quantity; }
        void set_authorised_quantity(const AuthorisedQuantity & value) { this->authorised_quantity = value; }

        const AuthorisedQuantity & get_average_price() const { return average_price; }
        AuthorisedQuantity & get_mutable_average_price() { return average_price; }
        void set_average_price(const AuthorisedQuantity & value) { this->average_price = value; }

        const AuthorisedQuantity & get_close_price() const { return close_price; }
        AuthorisedQuantity & get_mutable_close_price() { return close_price; }
        void set_close_price(const AuthorisedQuantity & value) { this->close_price = value; }

        const AuthorisedQuantity & get_collateral_quantity() const { return collateral_quantity; }
        AuthorisedQuantity & get_mutable_collateral_quantity() { return collateral_quantity; }
        void set_collateral_quantity(const AuthorisedQuantity & value) { this->collateral_quantity = value; }

        const AuthorisedQuantity & get_collateral_type() const { return collateral_type; }
        AuthorisedQuantity & get_mutable_collateral_type() { return collateral_type; }
        void set_collateral_type(const AuthorisedQuantity & value) { this->collateral_type = value; }

        const AuthorisedQuantity & get_day_change() const { return day_change; }
        AuthorisedQuantity & get_mutable_day_change() { return day_change; }
        void set_day_change(const AuthorisedQuantity & value) { this->day_change = value; }

        const AuthorisedQuantity & get_day_change_percentage() const { return day_change_percentage; }
        AuthorisedQuantity & get_mutable_day_change_percentage() { return day_change_percentage; }
        void set_day_change_percentage(const AuthorisedQuantity & value) { this->day_change_percentage = value; }

        const AuthorisedQuantity & get_discrepancy() const { return discrepancy; }
        AuthorisedQuantity & get_mutable_discrepancy() { return discrepancy; }
        void set_discrepancy(const AuthorisedQuantity & value) { this->discrepancy = value; }

        const AuthorisedQuantity & get_exchange() const { return exchange; }
        AuthorisedQuantity & get_mutable_exchange() { return exchange; }
        void set_exchange(const AuthorisedQuantity & value) { this->exchange = value; }

        const AuthorisedQuantity & get_instrument_token() const { return instrument_token; }
        AuthorisedQuantity & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const AuthorisedQuantity & value) { this->instrument_token = value; }

        const AuthorisedQuantity & get_isin() const { return isin; }
        AuthorisedQuantity & get_mutable_isin() { return isin; }
        void set_isin(const AuthorisedQuantity & value) { this->isin = value; }

        const AuthorisedQuantity & get_last_price() const { return last_price; }
        AuthorisedQuantity & get_mutable_last_price() { return last_price; }
        void set_last_price(const AuthorisedQuantity & value) { this->last_price = value; }

        const AuthorisedQuantity & get_opening_quantity() const { return opening_quantity; }
        AuthorisedQuantity & get_mutable_opening_quantity() { return opening_quantity; }
        void set_opening_quantity(const AuthorisedQuantity & value) { this->opening_quantity = value; }

        const AuthorisedQuantity & get_pnl() const { return pnl; }
        AuthorisedQuantity & get_mutable_pnl() { return pnl; }
        void set_pnl(const AuthorisedQuantity & value) { this->pnl = value; }

        const AuthorisedQuantity & get_price() const { return price; }
        AuthorisedQuantity & get_mutable_price() { return price; }
        void set_price(const AuthorisedQuantity & value) { this->price = value; }

        const AuthorisedQuantity & get_product() const { return product; }
        AuthorisedQuantity & get_mutable_product() { return product; }
        void set_product(const AuthorisedQuantity & value) { this->product = value; }

        const AuthorisedQuantity & get_quantity() const { return quantity; }
        AuthorisedQuantity & get_mutable_quantity() { return quantity; }
        void set_quantity(const AuthorisedQuantity & value) { this->quantity = value; }

        const AuthorisedQuantity & get_realised_quantity() const { return realised_quantity; }
        AuthorisedQuantity & get_mutable_realised_quantity() { return realised_quantity; }
        void set_realised_quantity(const AuthorisedQuantity & value) { this->realised_quantity = value; }

        const AuthorisedQuantity & get_t1_quantity() const { return t1_quantity; }
        AuthorisedQuantity & get_mutable_t1_quantity() { return t1_quantity; }
        void set_t1_quantity(const AuthorisedQuantity & value) { this->t1_quantity = value; }

        const AuthorisedQuantity & get_tradingsymbol() const { return tradingsymbol; }
        AuthorisedQuantity & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const AuthorisedQuantity & value) { this->tradingsymbol = value; }

        const AuthorisedQuantity & get_used_quantity() const { return used_quantity; }
        AuthorisedQuantity & get_mutable_used_quantity() { return used_quantity; }
        void set_used_quantity(const AuthorisedQuantity & value) { this->used_quantity = value; }
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

    class HoldingsProperties {
        public:
        HoldingsProperties() = default;
        virtual ~HoldingsProperties() = default;

        private:
        Data data;
        AuthorisedQuantity status;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const AuthorisedQuantity & get_status() const { return status; }
        AuthorisedQuantity & get_mutable_status() { return status; }
        void set_status(const AuthorisedQuantity & value) { this->status = value; }
    };

    class HoldingsClass {
        public:
        HoldingsClass() = default;
        virtual ~HoldingsClass() = default;

        private:
        bool additional_properties;
        HoldingsProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const HoldingsProperties & get_properties() const { return properties; }
        HoldingsProperties & get_mutable_properties() { return properties; }
        void set_properties(const HoldingsProperties & value) { this->properties = value; }

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
        HoldingsClass holdings;

        public:
        const Datum & get_datum() const { return datum; }
        Datum & get_mutable_datum() { return datum; }
        void set_datum(const Datum & value) { this->datum = value; }

        const HoldingsClass & get_holdings() const { return holdings; }
        HoldingsClass & get_mutable_holdings() { return holdings; }
        void set_holdings(const HoldingsClass & value) { this->holdings = value; }
    };

    class Holdings {
        public:
        Holdings() = default;
        virtual ~Holdings() = default;

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
    void from_json(const json & j, quicktype::AuthorisedDate & x);
    void to_json(json & j, const quicktype::AuthorisedDate & x);

    void from_json(const json & j, quicktype::AuthorisedQuantity & x);
    void to_json(json & j, const quicktype::AuthorisedQuantity & x);

    void from_json(const json & j, quicktype::DatumProperties & x);
    void to_json(json & j, const quicktype::DatumProperties & x);

    void from_json(const json & j, quicktype::Datum & x);
    void to_json(json & j, const quicktype::Datum & x);

    void from_json(const json & j, quicktype::Items & x);
    void to_json(json & j, const quicktype::Items & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::HoldingsProperties & x);
    void to_json(json & j, const quicktype::HoldingsProperties & x);

    void from_json(const json & j, quicktype::HoldingsClass & x);
    void to_json(json & j, const quicktype::HoldingsClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::Holdings & x);
    void to_json(json & j, const quicktype::Holdings & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::AuthorisedDate& x) {
        x.set_format(j.at("format").get<std::string>());
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::AuthorisedDate & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::AuthorisedQuantity& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::AuthorisedQuantity & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DatumProperties& x) {
        x.set_authorised_date(j.at("authorised_date").get<quicktype::AuthorisedDate>());
        x.set_authorised_quantity(j.at("authorised_quantity").get<quicktype::AuthorisedQuantity>());
        x.set_average_price(j.at("average_price").get<quicktype::AuthorisedQuantity>());
        x.set_close_price(j.at("close_price").get<quicktype::AuthorisedQuantity>());
        x.set_collateral_quantity(j.at("collateral_quantity").get<quicktype::AuthorisedQuantity>());
        x.set_collateral_type(j.at("collateral_type").get<quicktype::AuthorisedQuantity>());
        x.set_day_change(j.at("day_change").get<quicktype::AuthorisedQuantity>());
        x.set_day_change_percentage(j.at("day_change_percentage").get<quicktype::AuthorisedQuantity>());
        x.set_discrepancy(j.at("discrepancy").get<quicktype::AuthorisedQuantity>());
        x.set_exchange(j.at("exchange").get<quicktype::AuthorisedQuantity>());
        x.set_instrument_token(j.at("instrument_token").get<quicktype::AuthorisedQuantity>());
        x.set_isin(j.at("isin").get<quicktype::AuthorisedQuantity>());
        x.set_last_price(j.at("last_price").get<quicktype::AuthorisedQuantity>());
        x.set_opening_quantity(j.at("opening_quantity").get<quicktype::AuthorisedQuantity>());
        x.set_pnl(j.at("pnl").get<quicktype::AuthorisedQuantity>());
        x.set_price(j.at("price").get<quicktype::AuthorisedQuantity>());
        x.set_product(j.at("product").get<quicktype::AuthorisedQuantity>());
        x.set_quantity(j.at("quantity").get<quicktype::AuthorisedQuantity>());
        x.set_realised_quantity(j.at("realised_quantity").get<quicktype::AuthorisedQuantity>());
        x.set_t1_quantity(j.at("t1_quantity").get<quicktype::AuthorisedQuantity>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::AuthorisedQuantity>());
        x.set_used_quantity(j.at("used_quantity").get<quicktype::AuthorisedQuantity>());
    }

    inline void to_json(json & j, const quicktype::DatumProperties & x) {
        j = json::object();
        j["authorised_date"] = x.get_authorised_date();
        j["authorised_quantity"] = x.get_authorised_quantity();
        j["average_price"] = x.get_average_price();
        j["close_price"] = x.get_close_price();
        j["collateral_quantity"] = x.get_collateral_quantity();
        j["collateral_type"] = x.get_collateral_type();
        j["day_change"] = x.get_day_change();
        j["day_change_percentage"] = x.get_day_change_percentage();
        j["discrepancy"] = x.get_discrepancy();
        j["exchange"] = x.get_exchange();
        j["instrument_token"] = x.get_instrument_token();
        j["isin"] = x.get_isin();
        j["last_price"] = x.get_last_price();
        j["opening_quantity"] = x.get_opening_quantity();
        j["pnl"] = x.get_pnl();
        j["price"] = x.get_price();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["realised_quantity"] = x.get_realised_quantity();
        j["t1_quantity"] = x.get_t1_quantity();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["used_quantity"] = x.get_used_quantity();
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

    inline void from_json(const json & j, quicktype::HoldingsProperties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
        x.set_status(j.at("status").get<quicktype::AuthorisedQuantity>());
    }

    inline void to_json(json & j, const quicktype::HoldingsProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::HoldingsClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::HoldingsProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::HoldingsClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_datum(j.at("Datum").get<quicktype::Datum>());
        x.set_holdings(j.at("Holdings").get<quicktype::HoldingsClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Datum"] = x.get_datum();
        j["Holdings"] = x.get_holdings();
    }

    inline void from_json(const json & j, quicktype::Holdings& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::Holdings & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }

    inline void from_json(const json & j, quicktype::Type & x) {
        if (j == "boolean") x = quicktype::Type::BOOLEAN;
        else if (j == "integer") x = quicktype::Type::INTEGER;
        else if (j == "number") x = quicktype::Type::NUMBER;
        else if (j == "string") x = quicktype::Type::STRING;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const quicktype::Type & x) {
        switch (x) {
            case quicktype::Type::BOOLEAN: j = "boolean"; break;
            case quicktype::Type::INTEGER: j = "integer"; break;
            case quicktype::Type::NUMBER: j = "number"; break;
            case quicktype::Type::STRING: j = "string"; break;
            default: throw "This should not happen";
        }
    }
}
