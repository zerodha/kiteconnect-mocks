//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     OrderInfo data = nlohmann::json::parse(jsonString);

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

    enum class Type : int { INTEGER, NUMBER, STRING, TYPE_NULL };

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

    class ExchangeOrderId {
        public:
        ExchangeOrderId() = default;
        virtual ~ExchangeOrderId() = default;

        private:
        std::vector<AveragePrice> any_of;

        public:
        const std::vector<AveragePrice> & get_any_of() const { return any_of; }
        std::vector<AveragePrice> & get_mutable_any_of() { return any_of; }
        void set_any_of(const std::vector<AveragePrice> & value) { this->any_of = value; }
    };

    class OrderTimestamp {
        public:
        OrderTimestamp() = default;
        virtual ~OrderTimestamp() = default;

        private:
        std::shared_ptr<std::string> format;
        Type type;

        public:
        std::shared_ptr<std::string> get_format() const { return format; }
        void set_format(std::shared_ptr<std::string> value) { this->format = value; }

        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class ExchangeTimestamp {
        public:
        ExchangeTimestamp() = default;
        virtual ~ExchangeTimestamp() = default;

        private:
        std::vector<OrderTimestamp> any_of;

        public:
        const std::vector<OrderTimestamp> & get_any_of() const { return any_of; }
        std::vector<OrderTimestamp> & get_mutable_any_of() { return any_of; }
        void set_any_of(const std::vector<OrderTimestamp> & value) { this->any_of = value; }
    };

    class DatumProperties {
        public:
        DatumProperties() = default;
        virtual ~DatumProperties() = default;

        private:
        AveragePrice average_price;
        AveragePrice cancelled_quantity;
        AveragePrice disclosed_quantity;
        AveragePrice exchange;
        ExchangeOrderId exchange_order_id;
        ExchangeTimestamp exchange_timestamp;
        AveragePrice filled_quantity;
        AveragePrice instrument_token;
        AveragePrice order_id;
        OrderTimestamp order_timestamp;
        AveragePrice order_type;
        AveragePrice parent_order_id;
        AveragePrice pending_quantity;
        AveragePrice placed_by;
        AveragePrice price;
        AveragePrice product;
        AveragePrice quantity;
        AveragePrice status;
        AveragePrice status_message;
        AveragePrice tag;
        AveragePrice tradingsymbol;
        AveragePrice transaction_type;
        AveragePrice trigger_price;
        AveragePrice validity;
        AveragePrice variety;

        public:
        const AveragePrice & get_average_price() const { return average_price; }
        AveragePrice & get_mutable_average_price() { return average_price; }
        void set_average_price(const AveragePrice & value) { this->average_price = value; }

        const AveragePrice & get_cancelled_quantity() const { return cancelled_quantity; }
        AveragePrice & get_mutable_cancelled_quantity() { return cancelled_quantity; }
        void set_cancelled_quantity(const AveragePrice & value) { this->cancelled_quantity = value; }

        const AveragePrice & get_disclosed_quantity() const { return disclosed_quantity; }
        AveragePrice & get_mutable_disclosed_quantity() { return disclosed_quantity; }
        void set_disclosed_quantity(const AveragePrice & value) { this->disclosed_quantity = value; }

        const AveragePrice & get_exchange() const { return exchange; }
        AveragePrice & get_mutable_exchange() { return exchange; }
        void set_exchange(const AveragePrice & value) { this->exchange = value; }

        const ExchangeOrderId & get_exchange_order_id() const { return exchange_order_id; }
        ExchangeOrderId & get_mutable_exchange_order_id() { return exchange_order_id; }
        void set_exchange_order_id(const ExchangeOrderId & value) { this->exchange_order_id = value; }

        const ExchangeTimestamp & get_exchange_timestamp() const { return exchange_timestamp; }
        ExchangeTimestamp & get_mutable_exchange_timestamp() { return exchange_timestamp; }
        void set_exchange_timestamp(const ExchangeTimestamp & value) { this->exchange_timestamp = value; }

        const AveragePrice & get_filled_quantity() const { return filled_quantity; }
        AveragePrice & get_mutable_filled_quantity() { return filled_quantity; }
        void set_filled_quantity(const AveragePrice & value) { this->filled_quantity = value; }

        const AveragePrice & get_instrument_token() const { return instrument_token; }
        AveragePrice & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const AveragePrice & value) { this->instrument_token = value; }

        const AveragePrice & get_order_id() const { return order_id; }
        AveragePrice & get_mutable_order_id() { return order_id; }
        void set_order_id(const AveragePrice & value) { this->order_id = value; }

        const OrderTimestamp & get_order_timestamp() const { return order_timestamp; }
        OrderTimestamp & get_mutable_order_timestamp() { return order_timestamp; }
        void set_order_timestamp(const OrderTimestamp & value) { this->order_timestamp = value; }

        const AveragePrice & get_order_type() const { return order_type; }
        AveragePrice & get_mutable_order_type() { return order_type; }
        void set_order_type(const AveragePrice & value) { this->order_type = value; }

        const AveragePrice & get_parent_order_id() const { return parent_order_id; }
        AveragePrice & get_mutable_parent_order_id() { return parent_order_id; }
        void set_parent_order_id(const AveragePrice & value) { this->parent_order_id = value; }

        const AveragePrice & get_pending_quantity() const { return pending_quantity; }
        AveragePrice & get_mutable_pending_quantity() { return pending_quantity; }
        void set_pending_quantity(const AveragePrice & value) { this->pending_quantity = value; }

        const AveragePrice & get_placed_by() const { return placed_by; }
        AveragePrice & get_mutable_placed_by() { return placed_by; }
        void set_placed_by(const AveragePrice & value) { this->placed_by = value; }

        const AveragePrice & get_price() const { return price; }
        AveragePrice & get_mutable_price() { return price; }
        void set_price(const AveragePrice & value) { this->price = value; }

        const AveragePrice & get_product() const { return product; }
        AveragePrice & get_mutable_product() { return product; }
        void set_product(const AveragePrice & value) { this->product = value; }

        const AveragePrice & get_quantity() const { return quantity; }
        AveragePrice & get_mutable_quantity() { return quantity; }
        void set_quantity(const AveragePrice & value) { this->quantity = value; }

        const AveragePrice & get_status() const { return status; }
        AveragePrice & get_mutable_status() { return status; }
        void set_status(const AveragePrice & value) { this->status = value; }

        const AveragePrice & get_status_message() const { return status_message; }
        AveragePrice & get_mutable_status_message() { return status_message; }
        void set_status_message(const AveragePrice & value) { this->status_message = value; }

        const AveragePrice & get_tag() const { return tag; }
        AveragePrice & get_mutable_tag() { return tag; }
        void set_tag(const AveragePrice & value) { this->tag = value; }

        const AveragePrice & get_tradingsymbol() const { return tradingsymbol; }
        AveragePrice & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const AveragePrice & value) { this->tradingsymbol = value; }

        const AveragePrice & get_transaction_type() const { return transaction_type; }
        AveragePrice & get_mutable_transaction_type() { return transaction_type; }
        void set_transaction_type(const AveragePrice & value) { this->transaction_type = value; }

        const AveragePrice & get_trigger_price() const { return trigger_price; }
        AveragePrice & get_mutable_trigger_price() { return trigger_price; }
        void set_trigger_price(const AveragePrice & value) { this->trigger_price = value; }

        const AveragePrice & get_validity() const { return validity; }
        AveragePrice & get_mutable_validity() { return validity; }
        void set_validity(const AveragePrice & value) { this->validity = value; }

        const AveragePrice & get_variety() const { return variety; }
        AveragePrice & get_mutable_variety() { return variety; }
        void set_variety(const AveragePrice & value) { this->variety = value; }
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

    class OrderInfoProperties {
        public:
        OrderInfoProperties() = default;
        virtual ~OrderInfoProperties() = default;

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

    class OrderInfoClass {
        public:
        OrderInfoClass() = default;
        virtual ~OrderInfoClass() = default;

        private:
        bool additional_properties;
        OrderInfoProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const OrderInfoProperties & get_properties() const { return properties; }
        OrderInfoProperties & get_mutable_properties() { return properties; }
        void set_properties(const OrderInfoProperties & value) { this->properties = value; }

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
        OrderInfoClass order_info;

        public:
        const Datum & get_datum() const { return datum; }
        Datum & get_mutable_datum() { return datum; }
        void set_datum(const Datum & value) { this->datum = value; }

        const OrderInfoClass & get_order_info() const { return order_info; }
        OrderInfoClass & get_mutable_order_info() { return order_info; }
        void set_order_info(const OrderInfoClass & value) { this->order_info = value; }
    };

    class OrderInfo {
        public:
        OrderInfo() = default;
        virtual ~OrderInfo() = default;

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

    void from_json(const json & j, quicktype::ExchangeOrderId & x);
    void to_json(json & j, const quicktype::ExchangeOrderId & x);

    void from_json(const json & j, quicktype::OrderTimestamp & x);
    void to_json(json & j, const quicktype::OrderTimestamp & x);

    void from_json(const json & j, quicktype::ExchangeTimestamp & x);
    void to_json(json & j, const quicktype::ExchangeTimestamp & x);

    void from_json(const json & j, quicktype::DatumProperties & x);
    void to_json(json & j, const quicktype::DatumProperties & x);

    void from_json(const json & j, quicktype::Datum & x);
    void to_json(json & j, const quicktype::Datum & x);

    void from_json(const json & j, quicktype::Items & x);
    void to_json(json & j, const quicktype::Items & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::OrderInfoProperties & x);
    void to_json(json & j, const quicktype::OrderInfoProperties & x);

    void from_json(const json & j, quicktype::OrderInfoClass & x);
    void to_json(json & j, const quicktype::OrderInfoClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::OrderInfo & x);
    void to_json(json & j, const quicktype::OrderInfo & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::AveragePrice& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::AveragePrice & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ExchangeOrderId& x) {
        x.set_any_of(j.at("anyOf").get<std::vector<quicktype::AveragePrice>>());
    }

    inline void to_json(json & j, const quicktype::ExchangeOrderId & x) {
        j = json::object();
        j["anyOf"] = x.get_any_of();
    }

    inline void from_json(const json & j, quicktype::OrderTimestamp& x) {
        x.set_format(quicktype::get_optional<std::string>(j, "format"));
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::OrderTimestamp & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ExchangeTimestamp& x) {
        x.set_any_of(j.at("anyOf").get<std::vector<quicktype::OrderTimestamp>>());
    }

    inline void to_json(json & j, const quicktype::ExchangeTimestamp & x) {
        j = json::object();
        j["anyOf"] = x.get_any_of();
    }

    inline void from_json(const json & j, quicktype::DatumProperties& x) {
        x.set_average_price(j.at("average_price").get<quicktype::AveragePrice>());
        x.set_cancelled_quantity(j.at("cancelled_quantity").get<quicktype::AveragePrice>());
        x.set_disclosed_quantity(j.at("disclosed_quantity").get<quicktype::AveragePrice>());
        x.set_exchange(j.at("exchange").get<quicktype::AveragePrice>());
        x.set_exchange_order_id(j.at("exchange_order_id").get<quicktype::ExchangeOrderId>());
        x.set_exchange_timestamp(j.at("exchange_timestamp").get<quicktype::ExchangeTimestamp>());
        x.set_filled_quantity(j.at("filled_quantity").get<quicktype::AveragePrice>());
        x.set_instrument_token(j.at("instrument_token").get<quicktype::AveragePrice>());
        x.set_order_id(j.at("order_id").get<quicktype::AveragePrice>());
        x.set_order_timestamp(j.at("order_timestamp").get<quicktype::OrderTimestamp>());
        x.set_order_type(j.at("order_type").get<quicktype::AveragePrice>());
        x.set_parent_order_id(j.at("parent_order_id").get<quicktype::AveragePrice>());
        x.set_pending_quantity(j.at("pending_quantity").get<quicktype::AveragePrice>());
        x.set_placed_by(j.at("placed_by").get<quicktype::AveragePrice>());
        x.set_price(j.at("price").get<quicktype::AveragePrice>());
        x.set_product(j.at("product").get<quicktype::AveragePrice>());
        x.set_quantity(j.at("quantity").get<quicktype::AveragePrice>());
        x.set_status(j.at("status").get<quicktype::AveragePrice>());
        x.set_status_message(j.at("status_message").get<quicktype::AveragePrice>());
        x.set_tag(j.at("tag").get<quicktype::AveragePrice>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::AveragePrice>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::AveragePrice>());
        x.set_trigger_price(j.at("trigger_price").get<quicktype::AveragePrice>());
        x.set_validity(j.at("validity").get<quicktype::AveragePrice>());
        x.set_variety(j.at("variety").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::DatumProperties & x) {
        j = json::object();
        j["average_price"] = x.get_average_price();
        j["cancelled_quantity"] = x.get_cancelled_quantity();
        j["disclosed_quantity"] = x.get_disclosed_quantity();
        j["exchange"] = x.get_exchange();
        j["exchange_order_id"] = x.get_exchange_order_id();
        j["exchange_timestamp"] = x.get_exchange_timestamp();
        j["filled_quantity"] = x.get_filled_quantity();
        j["instrument_token"] = x.get_instrument_token();
        j["order_id"] = x.get_order_id();
        j["order_timestamp"] = x.get_order_timestamp();
        j["order_type"] = x.get_order_type();
        j["parent_order_id"] = x.get_parent_order_id();
        j["pending_quantity"] = x.get_pending_quantity();
        j["placed_by"] = x.get_placed_by();
        j["price"] = x.get_price();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["status"] = x.get_status();
        j["status_message"] = x.get_status_message();
        j["tag"] = x.get_tag();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["trigger_price"] = x.get_trigger_price();
        j["validity"] = x.get_validity();
        j["variety"] = x.get_variety();
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

    inline void from_json(const json & j, quicktype::OrderInfoProperties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
        x.set_status(j.at("status").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::OrderInfoProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::OrderInfoClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::OrderInfoProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::OrderInfoClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_datum(j.at("Datum").get<quicktype::Datum>());
        x.set_order_info(j.at("OrderInfo").get<quicktype::OrderInfoClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Datum"] = x.get_datum();
        j["OrderInfo"] = x.get_order_info();
    }

    inline void from_json(const json & j, quicktype::OrderInfo& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::OrderInfo & x) {
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
