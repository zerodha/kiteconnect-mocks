//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Postback data = nlohmann::json::parse(jsonString);

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

    class Meta {
        public:
        Meta() = default;
        virtual ~Meta() = default;

        private:
        bool additional_properties;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const std::string & get_title() const { return title; }
        std::string & get_mutable_title() { return title; }
        void set_title(const std::string & value) { this->title = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    enum class Type : int { INTEGER, STRING, TYPE_NULL };

    class AppId {
        public:
        AppId() = default;
        virtual ~AppId() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class Timestamp {
        public:
        Timestamp() = default;
        virtual ~Timestamp() = default;

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

    class MetaClass {
        public:
        MetaClass() = default;
        virtual ~MetaClass() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    class Properties {
        public:
        Properties() = default;
        virtual ~Properties() = default;

        private:
        AppId app_id;
        AppId average_price;
        AppId cancelled_quantity;
        AppId checksum;
        AppId disclosed_quantity;
        AppId exchange;
        AppId exchange_order_id;
        Timestamp exchange_timestamp;
        Timestamp exchange_update_timestamp;
        AppId filled_quantity;
        AppId guid;
        AppId instrument_token;
        AppId market_protection;
        MetaClass meta;
        AppId order_id;
        Timestamp order_timestamp;
        AppId order_type;
        AppId parent_order_id;
        AppId pending_quantity;
        AppId placed_by;
        AppId price;
        AppId product;
        AppId quantity;
        AppId status;
        AppId status_message;
        AppId status_message_raw;
        AppId tag;
        AppId tradingsymbol;
        AppId transaction_type;
        AppId trigger_price;
        AppId unfilled_quantity;
        AppId user_id;
        AppId validity;
        AppId variety;

        public:
        const AppId & get_app_id() const { return app_id; }
        AppId & get_mutable_app_id() { return app_id; }
        void set_app_id(const AppId & value) { this->app_id = value; }

        const AppId & get_average_price() const { return average_price; }
        AppId & get_mutable_average_price() { return average_price; }
        void set_average_price(const AppId & value) { this->average_price = value; }

        const AppId & get_cancelled_quantity() const { return cancelled_quantity; }
        AppId & get_mutable_cancelled_quantity() { return cancelled_quantity; }
        void set_cancelled_quantity(const AppId & value) { this->cancelled_quantity = value; }

        const AppId & get_checksum() const { return checksum; }
        AppId & get_mutable_checksum() { return checksum; }
        void set_checksum(const AppId & value) { this->checksum = value; }

        const AppId & get_disclosed_quantity() const { return disclosed_quantity; }
        AppId & get_mutable_disclosed_quantity() { return disclosed_quantity; }
        void set_disclosed_quantity(const AppId & value) { this->disclosed_quantity = value; }

        const AppId & get_exchange() const { return exchange; }
        AppId & get_mutable_exchange() { return exchange; }
        void set_exchange(const AppId & value) { this->exchange = value; }

        const AppId & get_exchange_order_id() const { return exchange_order_id; }
        AppId & get_mutable_exchange_order_id() { return exchange_order_id; }
        void set_exchange_order_id(const AppId & value) { this->exchange_order_id = value; }

        const Timestamp & get_exchange_timestamp() const { return exchange_timestamp; }
        Timestamp & get_mutable_exchange_timestamp() { return exchange_timestamp; }
        void set_exchange_timestamp(const Timestamp & value) { this->exchange_timestamp = value; }

        const Timestamp & get_exchange_update_timestamp() const { return exchange_update_timestamp; }
        Timestamp & get_mutable_exchange_update_timestamp() { return exchange_update_timestamp; }
        void set_exchange_update_timestamp(const Timestamp & value) { this->exchange_update_timestamp = value; }

        const AppId & get_filled_quantity() const { return filled_quantity; }
        AppId & get_mutable_filled_quantity() { return filled_quantity; }
        void set_filled_quantity(const AppId & value) { this->filled_quantity = value; }

        const AppId & get_guid() const { return guid; }
        AppId & get_mutable_guid() { return guid; }
        void set_guid(const AppId & value) { this->guid = value; }

        const AppId & get_instrument_token() const { return instrument_token; }
        AppId & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const AppId & value) { this->instrument_token = value; }

        const AppId & get_market_protection() const { return market_protection; }
        AppId & get_mutable_market_protection() { return market_protection; }
        void set_market_protection(const AppId & value) { this->market_protection = value; }

        const MetaClass & get_meta() const { return meta; }
        MetaClass & get_mutable_meta() { return meta; }
        void set_meta(const MetaClass & value) { this->meta = value; }

        const AppId & get_order_id() const { return order_id; }
        AppId & get_mutable_order_id() { return order_id; }
        void set_order_id(const AppId & value) { this->order_id = value; }

        const Timestamp & get_order_timestamp() const { return order_timestamp; }
        Timestamp & get_mutable_order_timestamp() { return order_timestamp; }
        void set_order_timestamp(const Timestamp & value) { this->order_timestamp = value; }

        const AppId & get_order_type() const { return order_type; }
        AppId & get_mutable_order_type() { return order_type; }
        void set_order_type(const AppId & value) { this->order_type = value; }

        const AppId & get_parent_order_id() const { return parent_order_id; }
        AppId & get_mutable_parent_order_id() { return parent_order_id; }
        void set_parent_order_id(const AppId & value) { this->parent_order_id = value; }

        const AppId & get_pending_quantity() const { return pending_quantity; }
        AppId & get_mutable_pending_quantity() { return pending_quantity; }
        void set_pending_quantity(const AppId & value) { this->pending_quantity = value; }

        const AppId & get_placed_by() const { return placed_by; }
        AppId & get_mutable_placed_by() { return placed_by; }
        void set_placed_by(const AppId & value) { this->placed_by = value; }

        const AppId & get_price() const { return price; }
        AppId & get_mutable_price() { return price; }
        void set_price(const AppId & value) { this->price = value; }

        const AppId & get_product() const { return product; }
        AppId & get_mutable_product() { return product; }
        void set_product(const AppId & value) { this->product = value; }

        const AppId & get_quantity() const { return quantity; }
        AppId & get_mutable_quantity() { return quantity; }
        void set_quantity(const AppId & value) { this->quantity = value; }

        const AppId & get_status() const { return status; }
        AppId & get_mutable_status() { return status; }
        void set_status(const AppId & value) { this->status = value; }

        const AppId & get_status_message() const { return status_message; }
        AppId & get_mutable_status_message() { return status_message; }
        void set_status_message(const AppId & value) { this->status_message = value; }

        const AppId & get_status_message_raw() const { return status_message_raw; }
        AppId & get_mutable_status_message_raw() { return status_message_raw; }
        void set_status_message_raw(const AppId & value) { this->status_message_raw = value; }

        const AppId & get_tag() const { return tag; }
        AppId & get_mutable_tag() { return tag; }
        void set_tag(const AppId & value) { this->tag = value; }

        const AppId & get_tradingsymbol() const { return tradingsymbol; }
        AppId & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const AppId & value) { this->tradingsymbol = value; }

        const AppId & get_transaction_type() const { return transaction_type; }
        AppId & get_mutable_transaction_type() { return transaction_type; }
        void set_transaction_type(const AppId & value) { this->transaction_type = value; }

        const AppId & get_trigger_price() const { return trigger_price; }
        AppId & get_mutable_trigger_price() { return trigger_price; }
        void set_trigger_price(const AppId & value) { this->trigger_price = value; }

        const AppId & get_unfilled_quantity() const { return unfilled_quantity; }
        AppId & get_mutable_unfilled_quantity() { return unfilled_quantity; }
        void set_unfilled_quantity(const AppId & value) { this->unfilled_quantity = value; }

        const AppId & get_user_id() const { return user_id; }
        AppId & get_mutable_user_id() { return user_id; }
        void set_user_id(const AppId & value) { this->user_id = value; }

        const AppId & get_validity() const { return validity; }
        AppId & get_mutable_validity() { return validity; }
        void set_validity(const AppId & value) { this->validity = value; }

        const AppId & get_variety() const { return variety; }
        AppId & get_mutable_variety() { return variety; }
        void set_variety(const AppId & value) { this->variety = value; }
    };

    class PostbackClass {
        public:
        PostbackClass() = default;
        virtual ~PostbackClass() = default;

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
        Meta meta;
        PostbackClass postback;

        public:
        const Meta & get_meta() const { return meta; }
        Meta & get_mutable_meta() { return meta; }
        void set_meta(const Meta & value) { this->meta = value; }

        const PostbackClass & get_postback() const { return postback; }
        PostbackClass & get_mutable_postback() { return postback; }
        void set_postback(const PostbackClass & value) { this->postback = value; }
    };

    class Postback {
        public:
        Postback() = default;
        virtual ~Postback() = default;

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
    void from_json(const json & j, quicktype::Meta & x);
    void to_json(json & j, const quicktype::Meta & x);

    void from_json(const json & j, quicktype::AppId & x);
    void to_json(json & j, const quicktype::AppId & x);

    void from_json(const json & j, quicktype::Timestamp & x);
    void to_json(json & j, const quicktype::Timestamp & x);

    void from_json(const json & j, quicktype::MetaClass & x);
    void to_json(json & j, const quicktype::MetaClass & x);

    void from_json(const json & j, quicktype::Properties & x);
    void to_json(json & j, const quicktype::Properties & x);

    void from_json(const json & j, quicktype::PostbackClass & x);
    void to_json(json & j, const quicktype::PostbackClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::Postback & x);
    void to_json(json & j, const quicktype::Postback & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::Meta& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Meta & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::AppId& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::AppId & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Timestamp& x) {
        x.set_format(j.at("format").get<std::string>());
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::Timestamp & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::MetaClass& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MetaClass & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::Properties& x) {
        x.set_app_id(j.at("app_id").get<quicktype::AppId>());
        x.set_average_price(j.at("average_price").get<quicktype::AppId>());
        x.set_cancelled_quantity(j.at("cancelled_quantity").get<quicktype::AppId>());
        x.set_checksum(j.at("checksum").get<quicktype::AppId>());
        x.set_disclosed_quantity(j.at("disclosed_quantity").get<quicktype::AppId>());
        x.set_exchange(j.at("exchange").get<quicktype::AppId>());
        x.set_exchange_order_id(j.at("exchange_order_id").get<quicktype::AppId>());
        x.set_exchange_timestamp(j.at("exchange_timestamp").get<quicktype::Timestamp>());
        x.set_exchange_update_timestamp(j.at("exchange_update_timestamp").get<quicktype::Timestamp>());
        x.set_filled_quantity(j.at("filled_quantity").get<quicktype::AppId>());
        x.set_guid(j.at("guid").get<quicktype::AppId>());
        x.set_instrument_token(j.at("instrument_token").get<quicktype::AppId>());
        x.set_market_protection(j.at("market_protection").get<quicktype::AppId>());
        x.set_meta(j.at("meta").get<quicktype::MetaClass>());
        x.set_order_id(j.at("order_id").get<quicktype::AppId>());
        x.set_order_timestamp(j.at("order_timestamp").get<quicktype::Timestamp>());
        x.set_order_type(j.at("order_type").get<quicktype::AppId>());
        x.set_parent_order_id(j.at("parent_order_id").get<quicktype::AppId>());
        x.set_pending_quantity(j.at("pending_quantity").get<quicktype::AppId>());
        x.set_placed_by(j.at("placed_by").get<quicktype::AppId>());
        x.set_price(j.at("price").get<quicktype::AppId>());
        x.set_product(j.at("product").get<quicktype::AppId>());
        x.set_quantity(j.at("quantity").get<quicktype::AppId>());
        x.set_status(j.at("status").get<quicktype::AppId>());
        x.set_status_message(j.at("status_message").get<quicktype::AppId>());
        x.set_status_message_raw(j.at("status_message_raw").get<quicktype::AppId>());
        x.set_tag(j.at("tag").get<quicktype::AppId>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::AppId>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::AppId>());
        x.set_trigger_price(j.at("trigger_price").get<quicktype::AppId>());
        x.set_unfilled_quantity(j.at("unfilled_quantity").get<quicktype::AppId>());
        x.set_user_id(j.at("user_id").get<quicktype::AppId>());
        x.set_validity(j.at("validity").get<quicktype::AppId>());
        x.set_variety(j.at("variety").get<quicktype::AppId>());
    }

    inline void to_json(json & j, const quicktype::Properties & x) {
        j = json::object();
        j["app_id"] = x.get_app_id();
        j["average_price"] = x.get_average_price();
        j["cancelled_quantity"] = x.get_cancelled_quantity();
        j["checksum"] = x.get_checksum();
        j["disclosed_quantity"] = x.get_disclosed_quantity();
        j["exchange"] = x.get_exchange();
        j["exchange_order_id"] = x.get_exchange_order_id();
        j["exchange_timestamp"] = x.get_exchange_timestamp();
        j["exchange_update_timestamp"] = x.get_exchange_update_timestamp();
        j["filled_quantity"] = x.get_filled_quantity();
        j["guid"] = x.get_guid();
        j["instrument_token"] = x.get_instrument_token();
        j["market_protection"] = x.get_market_protection();
        j["meta"] = x.get_meta();
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
        j["status_message_raw"] = x.get_status_message_raw();
        j["tag"] = x.get_tag();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["trigger_price"] = x.get_trigger_price();
        j["unfilled_quantity"] = x.get_unfilled_quantity();
        j["user_id"] = x.get_user_id();
        j["validity"] = x.get_validity();
        j["variety"] = x.get_variety();
    }

    inline void from_json(const json & j, quicktype::PostbackClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::Properties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::PostbackClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_meta(j.at("Meta").get<quicktype::Meta>());
        x.set_postback(j.at("Postback").get<quicktype::PostbackClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Meta"] = x.get_meta();
        j["Postback"] = x.get_postback();
    }

    inline void from_json(const json & j, quicktype::Postback& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::Postback & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }

    inline void from_json(const json & j, quicktype::Type & x) {
        if (j == "integer") x = quicktype::Type::INTEGER;
        else if (j == "string") x = quicktype::Type::STRING;
        else if (j == "null") x = quicktype::Type::TYPE_NULL;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const quicktype::Type & x) {
        switch (x) {
            case quicktype::Type::INTEGER: j = "integer"; break;
            case quicktype::Type::STRING: j = "string"; break;
            case quicktype::Type::TYPE_NULL: j = "null"; break;
            default: throw "This should not happen";
        }
    }
}
