//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     OrderTrades data = nlohmann::json::parse(jsonString);

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

    enum class Type : int { INTEGER, STRING };

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

    class ExchangeTimestamp {
        public:
        ExchangeTimestamp() = default;
        virtual ~ExchangeTimestamp() = default;

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

    class DatumProperties {
        public:
        DatumProperties() = default;
        virtual ~DatumProperties() = default;

        private:
        AveragePrice average_price;
        AveragePrice exchange;
        AveragePrice exchange_order_id;
        ExchangeTimestamp exchange_timestamp;
        ExchangeTimestamp fill_timestamp;
        AveragePrice instrument_token;
        AveragePrice order_id;
        ExchangeTimestamp order_timestamp;
        AveragePrice product;
        AveragePrice quantity;
        ExchangeTimestamp trade_id;
        AveragePrice tradingsymbol;
        AveragePrice transaction_type;

        public:
        const AveragePrice & get_average_price() const { return average_price; }
        AveragePrice & get_mutable_average_price() { return average_price; }
        void set_average_price(const AveragePrice & value) { this->average_price = value; }

        const AveragePrice & get_exchange() const { return exchange; }
        AveragePrice & get_mutable_exchange() { return exchange; }
        void set_exchange(const AveragePrice & value) { this->exchange = value; }

        const AveragePrice & get_exchange_order_id() const { return exchange_order_id; }
        AveragePrice & get_mutable_exchange_order_id() { return exchange_order_id; }
        void set_exchange_order_id(const AveragePrice & value) { this->exchange_order_id = value; }

        const ExchangeTimestamp & get_exchange_timestamp() const { return exchange_timestamp; }
        ExchangeTimestamp & get_mutable_exchange_timestamp() { return exchange_timestamp; }
        void set_exchange_timestamp(const ExchangeTimestamp & value) { this->exchange_timestamp = value; }

        const ExchangeTimestamp & get_fill_timestamp() const { return fill_timestamp; }
        ExchangeTimestamp & get_mutable_fill_timestamp() { return fill_timestamp; }
        void set_fill_timestamp(const ExchangeTimestamp & value) { this->fill_timestamp = value; }

        const AveragePrice & get_instrument_token() const { return instrument_token; }
        AveragePrice & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const AveragePrice & value) { this->instrument_token = value; }

        const AveragePrice & get_order_id() const { return order_id; }
        AveragePrice & get_mutable_order_id() { return order_id; }
        void set_order_id(const AveragePrice & value) { this->order_id = value; }

        const ExchangeTimestamp & get_order_timestamp() const { return order_timestamp; }
        ExchangeTimestamp & get_mutable_order_timestamp() { return order_timestamp; }
        void set_order_timestamp(const ExchangeTimestamp & value) { this->order_timestamp = value; }

        const AveragePrice & get_product() const { return product; }
        AveragePrice & get_mutable_product() { return product; }
        void set_product(const AveragePrice & value) { this->product = value; }

        const AveragePrice & get_quantity() const { return quantity; }
        AveragePrice & get_mutable_quantity() { return quantity; }
        void set_quantity(const AveragePrice & value) { this->quantity = value; }

        const ExchangeTimestamp & get_trade_id() const { return trade_id; }
        ExchangeTimestamp & get_mutable_trade_id() { return trade_id; }
        void set_trade_id(const ExchangeTimestamp & value) { this->trade_id = value; }

        const AveragePrice & get_tradingsymbol() const { return tradingsymbol; }
        AveragePrice & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const AveragePrice & value) { this->tradingsymbol = value; }

        const AveragePrice & get_transaction_type() const { return transaction_type; }
        AveragePrice & get_mutable_transaction_type() { return transaction_type; }
        void set_transaction_type(const AveragePrice & value) { this->transaction_type = value; }
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

    class OrderTradesProperties {
        public:
        OrderTradesProperties() = default;
        virtual ~OrderTradesProperties() = default;

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

    class OrderTradesClass {
        public:
        OrderTradesClass() = default;
        virtual ~OrderTradesClass() = default;

        private:
        bool additional_properties;
        OrderTradesProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const OrderTradesProperties & get_properties() const { return properties; }
        OrderTradesProperties & get_mutable_properties() { return properties; }
        void set_properties(const OrderTradesProperties & value) { this->properties = value; }

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
        OrderTradesClass order_trades;

        public:
        const Datum & get_datum() const { return datum; }
        Datum & get_mutable_datum() { return datum; }
        void set_datum(const Datum & value) { this->datum = value; }

        const OrderTradesClass & get_order_trades() const { return order_trades; }
        OrderTradesClass & get_mutable_order_trades() { return order_trades; }
        void set_order_trades(const OrderTradesClass & value) { this->order_trades = value; }
    };

    class OrderTrades {
        public:
        OrderTrades() = default;
        virtual ~OrderTrades() = default;

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

    void from_json(const json & j, quicktype::OrderTradesProperties & x);
    void to_json(json & j, const quicktype::OrderTradesProperties & x);

    void from_json(const json & j, quicktype::OrderTradesClass & x);
    void to_json(json & j, const quicktype::OrderTradesClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::OrderTrades & x);
    void to_json(json & j, const quicktype::OrderTrades & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::AveragePrice& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::AveragePrice & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ExchangeTimestamp& x) {
        x.set_format(j.at("format").get<std::string>());
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::ExchangeTimestamp & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DatumProperties& x) {
        x.set_average_price(j.at("average_price").get<quicktype::AveragePrice>());
        x.set_exchange(j.at("exchange").get<quicktype::AveragePrice>());
        x.set_exchange_order_id(j.at("exchange_order_id").get<quicktype::AveragePrice>());
        x.set_exchange_timestamp(j.at("exchange_timestamp").get<quicktype::ExchangeTimestamp>());
        x.set_fill_timestamp(j.at("fill_timestamp").get<quicktype::ExchangeTimestamp>());
        x.set_instrument_token(j.at("instrument_token").get<quicktype::AveragePrice>());
        x.set_order_id(j.at("order_id").get<quicktype::AveragePrice>());
        x.set_order_timestamp(j.at("order_timestamp").get<quicktype::ExchangeTimestamp>());
        x.set_product(j.at("product").get<quicktype::AveragePrice>());
        x.set_quantity(j.at("quantity").get<quicktype::AveragePrice>());
        x.set_trade_id(j.at("trade_id").get<quicktype::ExchangeTimestamp>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::AveragePrice>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::DatumProperties & x) {
        j = json::object();
        j["average_price"] = x.get_average_price();
        j["exchange"] = x.get_exchange();
        j["exchange_order_id"] = x.get_exchange_order_id();
        j["exchange_timestamp"] = x.get_exchange_timestamp();
        j["fill_timestamp"] = x.get_fill_timestamp();
        j["instrument_token"] = x.get_instrument_token();
        j["order_id"] = x.get_order_id();
        j["order_timestamp"] = x.get_order_timestamp();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["trade_id"] = x.get_trade_id();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
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

    inline void from_json(const json & j, quicktype::OrderTradesProperties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
        x.set_status(j.at("status").get<quicktype::AveragePrice>());
    }

    inline void to_json(json & j, const quicktype::OrderTradesProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::OrderTradesClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::OrderTradesProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::OrderTradesClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_datum(j.at("Datum").get<quicktype::Datum>());
        x.set_order_trades(j.at("OrderTrades").get<quicktype::OrderTradesClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Datum"] = x.get_datum();
        j["OrderTrades"] = x.get_order_trades();
    }

    inline void from_json(const json & j, quicktype::OrderTrades& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::OrderTrades & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }

    inline void from_json(const json & j, quicktype::Type & x) {
        if (j == "integer") x = quicktype::Type::INTEGER;
        else if (j == "string") x = quicktype::Type::STRING;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const quicktype::Type & x) {
        switch (x) {
            case quicktype::Type::INTEGER: j = "integer"; break;
            case quicktype::Type::STRING: j = "string"; break;
            default: throw "This should not happen";
        }
    }
}
