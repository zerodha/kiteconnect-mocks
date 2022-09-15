//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     GttGetOrders data = nlohmann::json::parse(jsonString);

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

    class Exchange {
        public:
        Exchange() = default;
        virtual ~Exchange() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class TriggerValues {
        public:
        TriggerValues() = default;
        virtual ~TriggerValues() = default;

        private:
        Exchange items;
        std::string type;

        public:
        const Exchange & get_items() const { return items; }
        Exchange & get_mutable_items() { return items; }
        void set_items(const Exchange & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class ConditionProperties {
        public:
        ConditionProperties() = default;
        virtual ~ConditionProperties() = default;

        private:
        Exchange exchange;
        Exchange instrument_token;
        Exchange last_price;
        Exchange tradingsymbol;
        TriggerValues trigger_values;

        public:
        const Exchange & get_exchange() const { return exchange; }
        Exchange & get_mutable_exchange() { return exchange; }
        void set_exchange(const Exchange & value) { this->exchange = value; }

        const Exchange & get_instrument_token() const { return instrument_token; }
        Exchange & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const Exchange & value) { this->instrument_token = value; }

        const Exchange & get_last_price() const { return last_price; }
        Exchange & get_mutable_last_price() { return last_price; }
        void set_last_price(const Exchange & value) { this->last_price = value; }

        const Exchange & get_tradingsymbol() const { return tradingsymbol; }
        Exchange & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const Exchange & value) { this->tradingsymbol = value; }

        const TriggerValues & get_trigger_values() const { return trigger_values; }
        TriggerValues & get_mutable_trigger_values() { return trigger_values; }
        void set_trigger_values(const TriggerValues & value) { this->trigger_values = value; }
    };

    class Condition {
        public:
        Condition() = default;
        virtual ~Condition() = default;

        private:
        bool additional_properties;
        ConditionProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const ConditionProperties & get_properties() const { return properties; }
        ConditionProperties & get_mutable_properties() { return properties; }
        void set_properties(const ConditionProperties & value) { this->properties = value; }

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

    class ConditionClass {
        public:
        ConditionClass() = default;
        virtual ~ConditionClass() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    class CreatedAt {
        public:
        CreatedAt() = default;
        virtual ~CreatedAt() = default;

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

    class AnyOf {
        public:
        AnyOf() = default;
        virtual ~AnyOf() = default;

        private:
        std::shared_ptr<std::string> ref;
        std::shared_ptr<Type> type;

        public:
        std::shared_ptr<std::string> get_ref() const { return ref; }
        void set_ref(std::shared_ptr<std::string> value) { this->ref = value; }

        std::shared_ptr<Type> get_type() const { return type; }
        void set_type(std::shared_ptr<Type> value) { this->type = value; }
    };

    class Meta {
        public:
        Meta() = default;
        virtual ~Meta() = default;

        private:
        std::vector<AnyOf> any_of;

        public:
        const std::vector<AnyOf> & get_any_of() const { return any_of; }
        std::vector<AnyOf> & get_mutable_any_of() { return any_of; }
        void set_any_of(const std::vector<AnyOf> & value) { this->any_of = value; }
    };

    class Orders {
        public:
        Orders() = default;
        virtual ~Orders() = default;

        private:
        ConditionClass items;
        std::string type;

        public:
        const ConditionClass & get_items() const { return items; }
        ConditionClass & get_mutable_items() { return items; }
        void set_items(const ConditionClass & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class DatumProperties {
        public:
        DatumProperties() = default;
        virtual ~DatumProperties() = default;

        private:
        ConditionClass condition;
        CreatedAt created_at;
        CreatedAt expires_at;
        Exchange id;
        Meta meta;
        Orders orders;
        Exchange parent_trigger;
        Exchange status;
        Exchange type;
        CreatedAt updated_at;
        Exchange user_id;

        public:
        const ConditionClass & get_condition() const { return condition; }
        ConditionClass & get_mutable_condition() { return condition; }
        void set_condition(const ConditionClass & value) { this->condition = value; }

        const CreatedAt & get_created_at() const { return created_at; }
        CreatedAt & get_mutable_created_at() { return created_at; }
        void set_created_at(const CreatedAt & value) { this->created_at = value; }

        const CreatedAt & get_expires_at() const { return expires_at; }
        CreatedAt & get_mutable_expires_at() { return expires_at; }
        void set_expires_at(const CreatedAt & value) { this->expires_at = value; }

        const Exchange & get_id() const { return id; }
        Exchange & get_mutable_id() { return id; }
        void set_id(const Exchange & value) { this->id = value; }

        const Meta & get_meta() const { return meta; }
        Meta & get_mutable_meta() { return meta; }
        void set_meta(const Meta & value) { this->meta = value; }

        const Orders & get_orders() const { return orders; }
        Orders & get_mutable_orders() { return orders; }
        void set_orders(const Orders & value) { this->orders = value; }

        const Exchange & get_parent_trigger() const { return parent_trigger; }
        Exchange & get_mutable_parent_trigger() { return parent_trigger; }
        void set_parent_trigger(const Exchange & value) { this->parent_trigger = value; }

        const Exchange & get_status() const { return status; }
        Exchange & get_mutable_status() { return status; }
        void set_status(const Exchange & value) { this->status = value; }

        const Exchange & get_type() const { return type; }
        Exchange & get_mutable_type() { return type; }
        void set_type(const Exchange & value) { this->type = value; }

        const CreatedAt & get_updated_at() const { return updated_at; }
        CreatedAt & get_mutable_updated_at() { return updated_at; }
        void set_updated_at(const CreatedAt & value) { this->updated_at = value; }

        const Exchange & get_user_id() const { return user_id; }
        Exchange & get_mutable_user_id() { return user_id; }
        void set_user_id(const Exchange & value) { this->user_id = value; }
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

    class GttGetOrdersProperties {
        public:
        GttGetOrdersProperties() = default;
        virtual ~GttGetOrdersProperties() = default;

        private:
        Orders data;
        Exchange status;

        public:
        const Orders & get_data() const { return data; }
        Orders & get_mutable_data() { return data; }
        void set_data(const Orders & value) { this->data = value; }

        const Exchange & get_status() const { return status; }
        Exchange & get_mutable_status() { return status; }
        void set_status(const Exchange & value) { this->status = value; }
    };

    class GttGetOrdersClass {
        public:
        GttGetOrdersClass() = default;
        virtual ~GttGetOrdersClass() = default;

        private:
        bool additional_properties;
        GttGetOrdersProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const GttGetOrdersProperties & get_properties() const { return properties; }
        GttGetOrdersProperties & get_mutable_properties() { return properties; }
        void set_properties(const GttGetOrdersProperties & value) { this->properties = value; }

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

    class MetaClass {
        public:
        MetaClass() = default;
        virtual ~MetaClass() = default;

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

    class OrderProperties {
        public:
        OrderProperties() = default;
        virtual ~OrderProperties() = default;

        private:
        Exchange exchange;
        Exchange order_type;
        Exchange price;
        Exchange product;
        Exchange quantity;
        Meta result;
        Exchange tradingsymbol;
        Exchange transaction_type;

        public:
        const Exchange & get_exchange() const { return exchange; }
        Exchange & get_mutable_exchange() { return exchange; }
        void set_exchange(const Exchange & value) { this->exchange = value; }

        const Exchange & get_order_type() const { return order_type; }
        Exchange & get_mutable_order_type() { return order_type; }
        void set_order_type(const Exchange & value) { this->order_type = value; }

        const Exchange & get_price() const { return price; }
        Exchange & get_mutable_price() { return price; }
        void set_price(const Exchange & value) { this->price = value; }

        const Exchange & get_product() const { return product; }
        Exchange & get_mutable_product() { return product; }
        void set_product(const Exchange & value) { this->product = value; }

        const Exchange & get_quantity() const { return quantity; }
        Exchange & get_mutable_quantity() { return quantity; }
        void set_quantity(const Exchange & value) { this->quantity = value; }

        const Meta & get_result() const { return result; }
        Meta & get_mutable_result() { return result; }
        void set_result(const Meta & value) { this->result = value; }

        const Exchange & get_tradingsymbol() const { return tradingsymbol; }
        Exchange & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const Exchange & value) { this->tradingsymbol = value; }

        const Exchange & get_transaction_type() const { return transaction_type; }
        Exchange & get_mutable_transaction_type() { return transaction_type; }
        void set_transaction_type(const Exchange & value) { this->transaction_type = value; }
    };

    class Order {
        public:
        Order() = default;
        virtual ~Order() = default;

        private:
        bool additional_properties;
        OrderProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const OrderProperties & get_properties() const { return properties; }
        OrderProperties & get_mutable_properties() { return properties; }
        void set_properties(const OrderProperties & value) { this->properties = value; }

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

    class OrderResultProperties {
        public:
        OrderResultProperties() = default;
        virtual ~OrderResultProperties() = default;

        private:
        Exchange order_id;
        Exchange rejection_reason;
        Exchange status;

        public:
        const Exchange & get_order_id() const { return order_id; }
        Exchange & get_mutable_order_id() { return order_id; }
        void set_order_id(const Exchange & value) { this->order_id = value; }

        const Exchange & get_rejection_reason() const { return rejection_reason; }
        Exchange & get_mutable_rejection_reason() { return rejection_reason; }
        void set_rejection_reason(const Exchange & value) { this->rejection_reason = value; }

        const Exchange & get_status() const { return status; }
        Exchange & get_mutable_status() { return status; }
        void set_status(const Exchange & value) { this->status = value; }
    };

    class OrderResult {
        public:
        OrderResult() = default;
        virtual ~OrderResult() = default;

        private:
        bool additional_properties;
        OrderResultProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const OrderResultProperties & get_properties() const { return properties; }
        OrderResultProperties & get_mutable_properties() { return properties; }
        void set_properties(const OrderResultProperties & value) { this->properties = value; }

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

    class ResultProperties {
        public:
        ResultProperties() = default;
        virtual ~ResultProperties() = default;

        private:
        Exchange account_id;
        Exchange exchange;
        Exchange meta;
        ConditionClass order_result;
        Exchange order_type;
        Exchange price;
        Exchange product;
        Exchange quantity;
        CreatedAt timestamp;
        Exchange tradingsymbol;
        Exchange transaction_type;
        Exchange triggered_at;
        Exchange validity;

        public:
        const Exchange & get_account_id() const { return account_id; }
        Exchange & get_mutable_account_id() { return account_id; }
        void set_account_id(const Exchange & value) { this->account_id = value; }

        const Exchange & get_exchange() const { return exchange; }
        Exchange & get_mutable_exchange() { return exchange; }
        void set_exchange(const Exchange & value) { this->exchange = value; }

        const Exchange & get_meta() const { return meta; }
        Exchange & get_mutable_meta() { return meta; }
        void set_meta(const Exchange & value) { this->meta = value; }

        const ConditionClass & get_order_result() const { return order_result; }
        ConditionClass & get_mutable_order_result() { return order_result; }
        void set_order_result(const ConditionClass & value) { this->order_result = value; }

        const Exchange & get_order_type() const { return order_type; }
        Exchange & get_mutable_order_type() { return order_type; }
        void set_order_type(const Exchange & value) { this->order_type = value; }

        const Exchange & get_price() const { return price; }
        Exchange & get_mutable_price() { return price; }
        void set_price(const Exchange & value) { this->price = value; }

        const Exchange & get_product() const { return product; }
        Exchange & get_mutable_product() { return product; }
        void set_product(const Exchange & value) { this->product = value; }

        const Exchange & get_quantity() const { return quantity; }
        Exchange & get_mutable_quantity() { return quantity; }
        void set_quantity(const Exchange & value) { this->quantity = value; }

        const CreatedAt & get_timestamp() const { return timestamp; }
        CreatedAt & get_mutable_timestamp() { return timestamp; }
        void set_timestamp(const CreatedAt & value) { this->timestamp = value; }

        const Exchange & get_tradingsymbol() const { return tradingsymbol; }
        Exchange & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const Exchange & value) { this->tradingsymbol = value; }

        const Exchange & get_transaction_type() const { return transaction_type; }
        Exchange & get_mutable_transaction_type() { return transaction_type; }
        void set_transaction_type(const Exchange & value) { this->transaction_type = value; }

        const Exchange & get_triggered_at() const { return triggered_at; }
        Exchange & get_mutable_triggered_at() { return triggered_at; }
        void set_triggered_at(const Exchange & value) { this->triggered_at = value; }

        const Exchange & get_validity() const { return validity; }
        Exchange & get_mutable_validity() { return validity; }
        void set_validity(const Exchange & value) { this->validity = value; }
    };

    class Result {
        public:
        Result() = default;
        virtual ~Result() = default;

        private:
        bool additional_properties;
        ResultProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const ResultProperties & get_properties() const { return properties; }
        ResultProperties & get_mutable_properties() { return properties; }
        void set_properties(const ResultProperties & value) { this->properties = value; }

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
        Condition condition;
        Datum datum;
        GttGetOrdersClass gtt_get_orders;
        MetaClass meta;
        Order order;
        OrderResult order_result;
        Result result;

        public:
        const Condition & get_condition() const { return condition; }
        Condition & get_mutable_condition() { return condition; }
        void set_condition(const Condition & value) { this->condition = value; }

        const Datum & get_datum() const { return datum; }
        Datum & get_mutable_datum() { return datum; }
        void set_datum(const Datum & value) { this->datum = value; }

        const GttGetOrdersClass & get_gtt_get_orders() const { return gtt_get_orders; }
        GttGetOrdersClass & get_mutable_gtt_get_orders() { return gtt_get_orders; }
        void set_gtt_get_orders(const GttGetOrdersClass & value) { this->gtt_get_orders = value; }

        const MetaClass & get_meta() const { return meta; }
        MetaClass & get_mutable_meta() { return meta; }
        void set_meta(const MetaClass & value) { this->meta = value; }

        const Order & get_order() const { return order; }
        Order & get_mutable_order() { return order; }
        void set_order(const Order & value) { this->order = value; }

        const OrderResult & get_order_result() const { return order_result; }
        OrderResult & get_mutable_order_result() { return order_result; }
        void set_order_result(const OrderResult & value) { this->order_result = value; }

        const Result & get_result() const { return result; }
        Result & get_mutable_result() { return result; }
        void set_result(const Result & value) { this->result = value; }
    };

    class GttGetOrders {
        public:
        GttGetOrders() = default;
        virtual ~GttGetOrders() = default;

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
    void from_json(const json & j, quicktype::Exchange & x);
    void to_json(json & j, const quicktype::Exchange & x);

    void from_json(const json & j, quicktype::TriggerValues & x);
    void to_json(json & j, const quicktype::TriggerValues & x);

    void from_json(const json & j, quicktype::ConditionProperties & x);
    void to_json(json & j, const quicktype::ConditionProperties & x);

    void from_json(const json & j, quicktype::Condition & x);
    void to_json(json & j, const quicktype::Condition & x);

    void from_json(const json & j, quicktype::ConditionClass & x);
    void to_json(json & j, const quicktype::ConditionClass & x);

    void from_json(const json & j, quicktype::CreatedAt & x);
    void to_json(json & j, const quicktype::CreatedAt & x);

    void from_json(const json & j, quicktype::AnyOf & x);
    void to_json(json & j, const quicktype::AnyOf & x);

    void from_json(const json & j, quicktype::Meta & x);
    void to_json(json & j, const quicktype::Meta & x);

    void from_json(const json & j, quicktype::Orders & x);
    void to_json(json & j, const quicktype::Orders & x);

    void from_json(const json & j, quicktype::DatumProperties & x);
    void to_json(json & j, const quicktype::DatumProperties & x);

    void from_json(const json & j, quicktype::Datum & x);
    void to_json(json & j, const quicktype::Datum & x);

    void from_json(const json & j, quicktype::GttGetOrdersProperties & x);
    void to_json(json & j, const quicktype::GttGetOrdersProperties & x);

    void from_json(const json & j, quicktype::GttGetOrdersClass & x);
    void to_json(json & j, const quicktype::GttGetOrdersClass & x);

    void from_json(const json & j, quicktype::MetaClass & x);
    void to_json(json & j, const quicktype::MetaClass & x);

    void from_json(const json & j, quicktype::OrderProperties & x);
    void to_json(json & j, const quicktype::OrderProperties & x);

    void from_json(const json & j, quicktype::Order & x);
    void to_json(json & j, const quicktype::Order & x);

    void from_json(const json & j, quicktype::OrderResultProperties & x);
    void to_json(json & j, const quicktype::OrderResultProperties & x);

    void from_json(const json & j, quicktype::OrderResult & x);
    void to_json(json & j, const quicktype::OrderResult & x);

    void from_json(const json & j, quicktype::ResultProperties & x);
    void to_json(json & j, const quicktype::ResultProperties & x);

    void from_json(const json & j, quicktype::Result & x);
    void to_json(json & j, const quicktype::Result & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::GttGetOrders & x);
    void to_json(json & j, const quicktype::GttGetOrders & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::Exchange& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::Exchange & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::TriggerValues& x) {
        x.set_items(j.at("items").get<quicktype::Exchange>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::TriggerValues & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ConditionProperties& x) {
        x.set_exchange(j.at("exchange").get<quicktype::Exchange>());
        x.set_instrument_token(j.at("instrument_token").get<quicktype::Exchange>());
        x.set_last_price(j.at("last_price").get<quicktype::Exchange>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::Exchange>());
        x.set_trigger_values(j.at("trigger_values").get<quicktype::TriggerValues>());
    }

    inline void to_json(json & j, const quicktype::ConditionProperties & x) {
        j = json::object();
        j["exchange"] = x.get_exchange();
        j["instrument_token"] = x.get_instrument_token();
        j["last_price"] = x.get_last_price();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["trigger_values"] = x.get_trigger_values();
    }

    inline void from_json(const json & j, quicktype::Condition& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::ConditionProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Condition & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ConditionClass& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::ConditionClass & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::CreatedAt& x) {
        x.set_format(j.at("format").get<std::string>());
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::CreatedAt & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::AnyOf& x) {
        x.set_ref(quicktype::get_optional<std::string>(j, "$ref"));
        x.set_type(quicktype::get_optional<quicktype::Type>(j, "type"));
    }

    inline void to_json(json & j, const quicktype::AnyOf & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Meta& x) {
        x.set_any_of(j.at("anyOf").get<std::vector<quicktype::AnyOf>>());
    }

    inline void to_json(json & j, const quicktype::Meta & x) {
        j = json::object();
        j["anyOf"] = x.get_any_of();
    }

    inline void from_json(const json & j, quicktype::Orders& x) {
        x.set_items(j.at("items").get<quicktype::ConditionClass>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Orders & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DatumProperties& x) {
        x.set_condition(j.at("condition").get<quicktype::ConditionClass>());
        x.set_created_at(j.at("created_at").get<quicktype::CreatedAt>());
        x.set_expires_at(j.at("expires_at").get<quicktype::CreatedAt>());
        x.set_id(j.at("id").get<quicktype::Exchange>());
        x.set_meta(j.at("meta").get<quicktype::Meta>());
        x.set_orders(j.at("orders").get<quicktype::Orders>());
        x.set_parent_trigger(j.at("parent_trigger").get<quicktype::Exchange>());
        x.set_status(j.at("status").get<quicktype::Exchange>());
        x.set_type(j.at("type").get<quicktype::Exchange>());
        x.set_updated_at(j.at("updated_at").get<quicktype::CreatedAt>());
        x.set_user_id(j.at("user_id").get<quicktype::Exchange>());
    }

    inline void to_json(json & j, const quicktype::DatumProperties & x) {
        j = json::object();
        j["condition"] = x.get_condition();
        j["created_at"] = x.get_created_at();
        j["expires_at"] = x.get_expires_at();
        j["id"] = x.get_id();
        j["meta"] = x.get_meta();
        j["orders"] = x.get_orders();
        j["parent_trigger"] = x.get_parent_trigger();
        j["status"] = x.get_status();
        j["type"] = x.get_type();
        j["updated_at"] = x.get_updated_at();
        j["user_id"] = x.get_user_id();
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

    inline void from_json(const json & j, quicktype::GttGetOrdersProperties& x) {
        x.set_data(j.at("data").get<quicktype::Orders>());
        x.set_status(j.at("status").get<quicktype::Exchange>());
    }

    inline void to_json(json & j, const quicktype::GttGetOrdersProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::GttGetOrdersClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::GttGetOrdersProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::GttGetOrdersClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::MetaClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MetaClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::OrderProperties& x) {
        x.set_exchange(j.at("exchange").get<quicktype::Exchange>());
        x.set_order_type(j.at("order_type").get<quicktype::Exchange>());
        x.set_price(j.at("price").get<quicktype::Exchange>());
        x.set_product(j.at("product").get<quicktype::Exchange>());
        x.set_quantity(j.at("quantity").get<quicktype::Exchange>());
        x.set_result(j.at("result").get<quicktype::Meta>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::Exchange>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::Exchange>());
    }

    inline void to_json(json & j, const quicktype::OrderProperties & x) {
        j = json::object();
        j["exchange"] = x.get_exchange();
        j["order_type"] = x.get_order_type();
        j["price"] = x.get_price();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["result"] = x.get_result();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
    }

    inline void from_json(const json & j, quicktype::Order& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::OrderProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Order & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::OrderResultProperties& x) {
        x.set_order_id(j.at("order_id").get<quicktype::Exchange>());
        x.set_rejection_reason(j.at("rejection_reason").get<quicktype::Exchange>());
        x.set_status(j.at("status").get<quicktype::Exchange>());
    }

    inline void to_json(json & j, const quicktype::OrderResultProperties & x) {
        j = json::object();
        j["order_id"] = x.get_order_id();
        j["rejection_reason"] = x.get_rejection_reason();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::OrderResult& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::OrderResultProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::OrderResult & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ResultProperties& x) {
        x.set_account_id(j.at("account_id").get<quicktype::Exchange>());
        x.set_exchange(j.at("exchange").get<quicktype::Exchange>());
        x.set_meta(j.at("meta").get<quicktype::Exchange>());
        x.set_order_result(j.at("order_result").get<quicktype::ConditionClass>());
        x.set_order_type(j.at("order_type").get<quicktype::Exchange>());
        x.set_price(j.at("price").get<quicktype::Exchange>());
        x.set_product(j.at("product").get<quicktype::Exchange>());
        x.set_quantity(j.at("quantity").get<quicktype::Exchange>());
        x.set_timestamp(j.at("timestamp").get<quicktype::CreatedAt>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::Exchange>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::Exchange>());
        x.set_triggered_at(j.at("triggered_at").get<quicktype::Exchange>());
        x.set_validity(j.at("validity").get<quicktype::Exchange>());
    }

    inline void to_json(json & j, const quicktype::ResultProperties & x) {
        j = json::object();
        j["account_id"] = x.get_account_id();
        j["exchange"] = x.get_exchange();
        j["meta"] = x.get_meta();
        j["order_result"] = x.get_order_result();
        j["order_type"] = x.get_order_type();
        j["price"] = x.get_price();
        j["product"] = x.get_product();
        j["quantity"] = x.get_quantity();
        j["timestamp"] = x.get_timestamp();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["triggered_at"] = x.get_triggered_at();
        j["validity"] = x.get_validity();
    }

    inline void from_json(const json & j, quicktype::Result& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::ResultProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Result & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_condition(j.at("Condition").get<quicktype::Condition>());
        x.set_datum(j.at("Datum").get<quicktype::Datum>());
        x.set_gtt_get_orders(j.at("GttGetOrders").get<quicktype::GttGetOrdersClass>());
        x.set_meta(j.at("Meta").get<quicktype::MetaClass>());
        x.set_order(j.at("Order").get<quicktype::Order>());
        x.set_order_result(j.at("OrderResult").get<quicktype::OrderResult>());
        x.set_result(j.at("Result").get<quicktype::Result>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Condition"] = x.get_condition();
        j["Datum"] = x.get_datum();
        j["GttGetOrders"] = x.get_gtt_get_orders();
        j["Meta"] = x.get_meta();
        j["Order"] = x.get_order();
        j["OrderResult"] = x.get_order_result();
        j["Result"] = x.get_result();
    }

    inline void from_json(const json & j, quicktype::GttGetOrders& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::GttGetOrders & x) {
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
