//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     MfOrdersInfo data = nlohmann::json::parse(jsonString);

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

    enum class Type : int { INTEGER, NUMBER, STRING, TYPE_NULL };

    class Amount {
        public:
        Amount() = default;
        virtual ~Amount() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class LastPriceDate {
        public:
        LastPriceDate() = default;
        virtual ~LastPriceDate() = default;

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

    class DataProperties {
        public:
        DataProperties() = default;
        virtual ~DataProperties() = default;

        private:
        Amount amount;
        Amount average_price;
        Amount exchange_order_id;
        Amount exchange_timestamp;
        Amount folio;
        Amount fund;
        Amount last_price;
        LastPriceDate last_price_date;
        LastPriceDate order_id;
        LastPriceDate order_timestamp;
        Amount placed_by;
        Amount purchase_type;
        Amount quantity;
        Amount settlement_id;
        Amount status;
        Amount status_message;
        Amount tag;
        Amount tradingsymbol;
        Amount transaction_type;
        Amount variety;

        public:
        const Amount & get_amount() const { return amount; }
        Amount & get_mutable_amount() { return amount; }
        void set_amount(const Amount & value) { this->amount = value; }

        const Amount & get_average_price() const { return average_price; }
        Amount & get_mutable_average_price() { return average_price; }
        void set_average_price(const Amount & value) { this->average_price = value; }

        const Amount & get_exchange_order_id() const { return exchange_order_id; }
        Amount & get_mutable_exchange_order_id() { return exchange_order_id; }
        void set_exchange_order_id(const Amount & value) { this->exchange_order_id = value; }

        const Amount & get_exchange_timestamp() const { return exchange_timestamp; }
        Amount & get_mutable_exchange_timestamp() { return exchange_timestamp; }
        void set_exchange_timestamp(const Amount & value) { this->exchange_timestamp = value; }

        const Amount & get_folio() const { return folio; }
        Amount & get_mutable_folio() { return folio; }
        void set_folio(const Amount & value) { this->folio = value; }

        const Amount & get_fund() const { return fund; }
        Amount & get_mutable_fund() { return fund; }
        void set_fund(const Amount & value) { this->fund = value; }

        const Amount & get_last_price() const { return last_price; }
        Amount & get_mutable_last_price() { return last_price; }
        void set_last_price(const Amount & value) { this->last_price = value; }

        const LastPriceDate & get_last_price_date() const { return last_price_date; }
        LastPriceDate & get_mutable_last_price_date() { return last_price_date; }
        void set_last_price_date(const LastPriceDate & value) { this->last_price_date = value; }

        const LastPriceDate & get_order_id() const { return order_id; }
        LastPriceDate & get_mutable_order_id() { return order_id; }
        void set_order_id(const LastPriceDate & value) { this->order_id = value; }

        const LastPriceDate & get_order_timestamp() const { return order_timestamp; }
        LastPriceDate & get_mutable_order_timestamp() { return order_timestamp; }
        void set_order_timestamp(const LastPriceDate & value) { this->order_timestamp = value; }

        const Amount & get_placed_by() const { return placed_by; }
        Amount & get_mutable_placed_by() { return placed_by; }
        void set_placed_by(const Amount & value) { this->placed_by = value; }

        const Amount & get_purchase_type() const { return purchase_type; }
        Amount & get_mutable_purchase_type() { return purchase_type; }
        void set_purchase_type(const Amount & value) { this->purchase_type = value; }

        const Amount & get_quantity() const { return quantity; }
        Amount & get_mutable_quantity() { return quantity; }
        void set_quantity(const Amount & value) { this->quantity = value; }

        const Amount & get_settlement_id() const { return settlement_id; }
        Amount & get_mutable_settlement_id() { return settlement_id; }
        void set_settlement_id(const Amount & value) { this->settlement_id = value; }

        const Amount & get_status() const { return status; }
        Amount & get_mutable_status() { return status; }
        void set_status(const Amount & value) { this->status = value; }

        const Amount & get_status_message() const { return status_message; }
        Amount & get_mutable_status_message() { return status_message; }
        void set_status_message(const Amount & value) { this->status_message = value; }

        const Amount & get_tag() const { return tag; }
        Amount & get_mutable_tag() { return tag; }
        void set_tag(const Amount & value) { this->tag = value; }

        const Amount & get_tradingsymbol() const { return tradingsymbol; }
        Amount & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const Amount & value) { this->tradingsymbol = value; }

        const Amount & get_transaction_type() const { return transaction_type; }
        Amount & get_mutable_transaction_type() { return transaction_type; }
        void set_transaction_type(const Amount & value) { this->transaction_type = value; }

        const Amount & get_variety() const { return variety; }
        Amount & get_mutable_variety() { return variety; }
        void set_variety(const Amount & value) { this->variety = value; }
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

    class MfOrdersInfoProperties {
        public:
        MfOrdersInfoProperties() = default;
        virtual ~MfOrdersInfoProperties() = default;

        private:
        DataClass data;
        Amount status;

        public:
        const DataClass & get_data() const { return data; }
        DataClass & get_mutable_data() { return data; }
        void set_data(const DataClass & value) { this->data = value; }

        const Amount & get_status() const { return status; }
        Amount & get_mutable_status() { return status; }
        void set_status(const Amount & value) { this->status = value; }
    };

    class MfOrdersInfoClass {
        public:
        MfOrdersInfoClass() = default;
        virtual ~MfOrdersInfoClass() = default;

        private:
        bool additional_properties;
        MfOrdersInfoProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const MfOrdersInfoProperties & get_properties() const { return properties; }
        MfOrdersInfoProperties & get_mutable_properties() { return properties; }
        void set_properties(const MfOrdersInfoProperties & value) { this->properties = value; }

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
        MfOrdersInfoClass mf_orders_info;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const MfOrdersInfoClass & get_mf_orders_info() const { return mf_orders_info; }
        MfOrdersInfoClass & get_mutable_mf_orders_info() { return mf_orders_info; }
        void set_mf_orders_info(const MfOrdersInfoClass & value) { this->mf_orders_info = value; }
    };

    class MfOrdersInfo {
        public:
        MfOrdersInfo() = default;
        virtual ~MfOrdersInfo() = default;

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
    void from_json(const json & j, quicktype::Amount & x);
    void to_json(json & j, const quicktype::Amount & x);

    void from_json(const json & j, quicktype::LastPriceDate & x);
    void to_json(json & j, const quicktype::LastPriceDate & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::DataClass & x);
    void to_json(json & j, const quicktype::DataClass & x);

    void from_json(const json & j, quicktype::MfOrdersInfoProperties & x);
    void to_json(json & j, const quicktype::MfOrdersInfoProperties & x);

    void from_json(const json & j, quicktype::MfOrdersInfoClass & x);
    void to_json(json & j, const quicktype::MfOrdersInfoClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::MfOrdersInfo & x);
    void to_json(json & j, const quicktype::MfOrdersInfo & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::Amount& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::Amount & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::LastPriceDate& x) {
        x.set_format(j.at("format").get<std::string>());
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::LastPriceDate & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_amount(j.at("amount").get<quicktype::Amount>());
        x.set_average_price(j.at("average_price").get<quicktype::Amount>());
        x.set_exchange_order_id(j.at("exchange_order_id").get<quicktype::Amount>());
        x.set_exchange_timestamp(j.at("exchange_timestamp").get<quicktype::Amount>());
        x.set_folio(j.at("folio").get<quicktype::Amount>());
        x.set_fund(j.at("fund").get<quicktype::Amount>());
        x.set_last_price(j.at("last_price").get<quicktype::Amount>());
        x.set_last_price_date(j.at("last_price_date").get<quicktype::LastPriceDate>());
        x.set_order_id(j.at("order_id").get<quicktype::LastPriceDate>());
        x.set_order_timestamp(j.at("order_timestamp").get<quicktype::LastPriceDate>());
        x.set_placed_by(j.at("placed_by").get<quicktype::Amount>());
        x.set_purchase_type(j.at("purchase_type").get<quicktype::Amount>());
        x.set_quantity(j.at("quantity").get<quicktype::Amount>());
        x.set_settlement_id(j.at("settlement_id").get<quicktype::Amount>());
        x.set_status(j.at("status").get<quicktype::Amount>());
        x.set_status_message(j.at("status_message").get<quicktype::Amount>());
        x.set_tag(j.at("tag").get<quicktype::Amount>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::Amount>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::Amount>());
        x.set_variety(j.at("variety").get<quicktype::Amount>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["amount"] = x.get_amount();
        j["average_price"] = x.get_average_price();
        j["exchange_order_id"] = x.get_exchange_order_id();
        j["exchange_timestamp"] = x.get_exchange_timestamp();
        j["folio"] = x.get_folio();
        j["fund"] = x.get_fund();
        j["last_price"] = x.get_last_price();
        j["last_price_date"] = x.get_last_price_date();
        j["order_id"] = x.get_order_id();
        j["order_timestamp"] = x.get_order_timestamp();
        j["placed_by"] = x.get_placed_by();
        j["purchase_type"] = x.get_purchase_type();
        j["quantity"] = x.get_quantity();
        j["settlement_id"] = x.get_settlement_id();
        j["status"] = x.get_status();
        j["status_message"] = x.get_status_message();
        j["tag"] = x.get_tag();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["variety"] = x.get_variety();
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

    inline void from_json(const json & j, quicktype::MfOrdersInfoProperties& x) {
        x.set_data(j.at("data").get<quicktype::DataClass>());
        x.set_status(j.at("status").get<quicktype::Amount>());
    }

    inline void to_json(json & j, const quicktype::MfOrdersInfoProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::MfOrdersInfoClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::MfOrdersInfoProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MfOrdersInfoClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_mf_orders_info(j.at("MFOrdersInfo").get<quicktype::MfOrdersInfoClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Data"] = x.get_data();
        j["MFOrdersInfo"] = x.get_mf_orders_info();
    }

    inline void from_json(const json & j, quicktype::MfOrdersInfo& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::MfOrdersInfo & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }

    inline void from_json(const json & j, quicktype::Type & x) {
        if (j == "integer") x = quicktype::Type::INTEGER;
        else if (j == "number") x = quicktype::Type::NUMBER;
        else if (j == "string") x = quicktype::Type::STRING;
        else if (j == "null") x = quicktype::Type::TYPE_NULL;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const quicktype::Type & x) {
        switch (x) {
            case quicktype::Type::INTEGER: j = "integer"; break;
            case quicktype::Type::NUMBER: j = "number"; break;
            case quicktype::Type::STRING: j = "string"; break;
            case quicktype::Type::TYPE_NULL: j = "null"; break;
            default: throw "This should not happen";
        }
    }
}
