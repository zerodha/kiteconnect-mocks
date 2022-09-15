//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Quote data = nlohmann::json::parse(jsonString);

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

    class Orders {
        public:
        Orders() = default;
        virtual ~Orders() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class BuyProperties {
        public:
        BuyProperties() = default;
        virtual ~BuyProperties() = default;

        private:
        Orders orders;
        Orders price;
        Orders quantity;

        public:
        const Orders & get_orders() const { return orders; }
        Orders & get_mutable_orders() { return orders; }
        void set_orders(const Orders & value) { this->orders = value; }

        const Orders & get_price() const { return price; }
        Orders & get_mutable_price() { return price; }
        void set_price(const Orders & value) { this->price = value; }

        const Orders & get_quantity() const { return quantity; }
        Orders & get_mutable_quantity() { return quantity; }
        void set_quantity(const Orders & value) { this->quantity = value; }
    };

    class Buy {
        public:
        Buy() = default;
        virtual ~Buy() = default;

        private:
        bool additional_properties;
        BuyProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const BuyProperties & get_properties() const { return properties; }
        BuyProperties & get_mutable_properties() { return properties; }
        void set_properties(const BuyProperties & value) { this->properties = value; }

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

        public:
        const NseInfy & get_nse_infy() const { return nse_infy; }
        NseInfy & get_mutable_nse_infy() { return nse_infy; }
        void set_nse_infy(const NseInfy & value) { this->nse_infy = value; }
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

    class BuyClass {
        public:
        BuyClass() = default;
        virtual ~BuyClass() = default;

        private:
        NseInfy items;
        std::string type;

        public:
        const NseInfy & get_items() const { return items; }
        NseInfy & get_mutable_items() { return items; }
        void set_items(const NseInfy & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class DepthProperties {
        public:
        DepthProperties() = default;
        virtual ~DepthProperties() = default;

        private:
        BuyClass buy;
        BuyClass sell;

        public:
        const BuyClass & get_buy() const { return buy; }
        BuyClass & get_mutable_buy() { return buy; }
        void set_buy(const BuyClass & value) { this->buy = value; }

        const BuyClass & get_sell() const { return sell; }
        BuyClass & get_mutable_sell() { return sell; }
        void set_sell(const BuyClass & value) { this->sell = value; }
    };

    class Depth {
        public:
        Depth() = default;
        virtual ~Depth() = default;

        private:
        bool additional_properties;
        DepthProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const DepthProperties & get_properties() const { return properties; }
        DepthProperties & get_mutable_properties() { return properties; }
        void set_properties(const DepthProperties & value) { this->properties = value; }

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

    class LastTradeTime {
        public:
        LastTradeTime() = default;
        virtual ~LastTradeTime() = default;

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

    class NseInfyProperties {
        public:
        NseInfyProperties() = default;
        virtual ~NseInfyProperties() = default;

        private:
        Orders average_price;
        Orders buy_quantity;
        NseInfy depth;
        Orders instrument_token;
        Orders last_price;
        Orders last_quantity;
        LastTradeTime last_trade_time;
        Orders lower_circuit_limit;
        Orders net_change;
        NseInfy ohlc;
        Orders oi;
        Orders oi_day_high;
        Orders oi_day_low;
        Orders sell_quantity;
        LastTradeTime timestamp;
        Orders upper_circuit_limit;
        Orders volume;

        public:
        const Orders & get_average_price() const { return average_price; }
        Orders & get_mutable_average_price() { return average_price; }
        void set_average_price(const Orders & value) { this->average_price = value; }

        const Orders & get_buy_quantity() const { return buy_quantity; }
        Orders & get_mutable_buy_quantity() { return buy_quantity; }
        void set_buy_quantity(const Orders & value) { this->buy_quantity = value; }

        const NseInfy & get_depth() const { return depth; }
        NseInfy & get_mutable_depth() { return depth; }
        void set_depth(const NseInfy & value) { this->depth = value; }

        const Orders & get_instrument_token() const { return instrument_token; }
        Orders & get_mutable_instrument_token() { return instrument_token; }
        void set_instrument_token(const Orders & value) { this->instrument_token = value; }

        const Orders & get_last_price() const { return last_price; }
        Orders & get_mutable_last_price() { return last_price; }
        void set_last_price(const Orders & value) { this->last_price = value; }

        const Orders & get_last_quantity() const { return last_quantity; }
        Orders & get_mutable_last_quantity() { return last_quantity; }
        void set_last_quantity(const Orders & value) { this->last_quantity = value; }

        const LastTradeTime & get_last_trade_time() const { return last_trade_time; }
        LastTradeTime & get_mutable_last_trade_time() { return last_trade_time; }
        void set_last_trade_time(const LastTradeTime & value) { this->last_trade_time = value; }

        const Orders & get_lower_circuit_limit() const { return lower_circuit_limit; }
        Orders & get_mutable_lower_circuit_limit() { return lower_circuit_limit; }
        void set_lower_circuit_limit(const Orders & value) { this->lower_circuit_limit = value; }

        const Orders & get_net_change() const { return net_change; }
        Orders & get_mutable_net_change() { return net_change; }
        void set_net_change(const Orders & value) { this->net_change = value; }

        const NseInfy & get_ohlc() const { return ohlc; }
        NseInfy & get_mutable_ohlc() { return ohlc; }
        void set_ohlc(const NseInfy & value) { this->ohlc = value; }

        const Orders & get_oi() const { return oi; }
        Orders & get_mutable_oi() { return oi; }
        void set_oi(const Orders & value) { this->oi = value; }

        const Orders & get_oi_day_high() const { return oi_day_high; }
        Orders & get_mutable_oi_day_high() { return oi_day_high; }
        void set_oi_day_high(const Orders & value) { this->oi_day_high = value; }

        const Orders & get_oi_day_low() const { return oi_day_low; }
        Orders & get_mutable_oi_day_low() { return oi_day_low; }
        void set_oi_day_low(const Orders & value) { this->oi_day_low = value; }

        const Orders & get_sell_quantity() const { return sell_quantity; }
        Orders & get_mutable_sell_quantity() { return sell_quantity; }
        void set_sell_quantity(const Orders & value) { this->sell_quantity = value; }

        const LastTradeTime & get_timestamp() const { return timestamp; }
        LastTradeTime & get_mutable_timestamp() { return timestamp; }
        void set_timestamp(const LastTradeTime & value) { this->timestamp = value; }

        const Orders & get_upper_circuit_limit() const { return upper_circuit_limit; }
        Orders & get_mutable_upper_circuit_limit() { return upper_circuit_limit; }
        void set_upper_circuit_limit(const Orders & value) { this->upper_circuit_limit = value; }

        const Orders & get_volume() const { return volume; }
        Orders & get_mutable_volume() { return volume; }
        void set_volume(const Orders & value) { this->volume = value; }
    };

    class NseInfyClass {
        public:
        NseInfyClass() = default;
        virtual ~NseInfyClass() = default;

        private:
        bool additional_properties;
        NseInfyProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const NseInfyProperties & get_properties() const { return properties; }
        NseInfyProperties & get_mutable_properties() { return properties; }
        void set_properties(const NseInfyProperties & value) { this->properties = value; }

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

    class OhlcProperties {
        public:
        OhlcProperties() = default;
        virtual ~OhlcProperties() = default;

        private:
        Orders close;
        Orders high;
        Orders low;
        Orders open;

        public:
        const Orders & get_close() const { return close; }
        Orders & get_mutable_close() { return close; }
        void set_close(const Orders & value) { this->close = value; }

        const Orders & get_high() const { return high; }
        Orders & get_mutable_high() { return high; }
        void set_high(const Orders & value) { this->high = value; }

        const Orders & get_low() const { return low; }
        Orders & get_mutable_low() { return low; }
        void set_low(const Orders & value) { this->low = value; }

        const Orders & get_open() const { return open; }
        Orders & get_mutable_open() { return open; }
        void set_open(const Orders & value) { this->open = value; }
    };

    class Ohlc {
        public:
        Ohlc() = default;
        virtual ~Ohlc() = default;

        private:
        bool additional_properties;
        OhlcProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const OhlcProperties & get_properties() const { return properties; }
        OhlcProperties & get_mutable_properties() { return properties; }
        void set_properties(const OhlcProperties & value) { this->properties = value; }

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

    class QuoteProperties {
        public:
        QuoteProperties() = default;
        virtual ~QuoteProperties() = default;

        private:
        NseInfy data;
        Orders status;

        public:
        const NseInfy & get_data() const { return data; }
        NseInfy & get_mutable_data() { return data; }
        void set_data(const NseInfy & value) { this->data = value; }

        const Orders & get_status() const { return status; }
        Orders & get_mutable_status() { return status; }
        void set_status(const Orders & value) { this->status = value; }
    };

    class QuoteClass {
        public:
        QuoteClass() = default;
        virtual ~QuoteClass() = default;

        private:
        bool additional_properties;
        QuoteProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const QuoteProperties & get_properties() const { return properties; }
        QuoteProperties & get_mutable_properties() { return properties; }
        void set_properties(const QuoteProperties & value) { this->properties = value; }

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
        Buy buy;
        Data data;
        Depth depth;
        NseInfyClass nse_infy;
        Ohlc ohlc;
        QuoteClass quote;

        public:
        const Buy & get_buy() const { return buy; }
        Buy & get_mutable_buy() { return buy; }
        void set_buy(const Buy & value) { this->buy = value; }

        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const Depth & get_depth() const { return depth; }
        Depth & get_mutable_depth() { return depth; }
        void set_depth(const Depth & value) { this->depth = value; }

        const NseInfyClass & get_nse_infy() const { return nse_infy; }
        NseInfyClass & get_mutable_nse_infy() { return nse_infy; }
        void set_nse_infy(const NseInfyClass & value) { this->nse_infy = value; }

        const Ohlc & get_ohlc() const { return ohlc; }
        Ohlc & get_mutable_ohlc() { return ohlc; }
        void set_ohlc(const Ohlc & value) { this->ohlc = value; }

        const QuoteClass & get_quote() const { return quote; }
        QuoteClass & get_mutable_quote() { return quote; }
        void set_quote(const QuoteClass & value) { this->quote = value; }
    };

    class Quote {
        public:
        Quote() = default;
        virtual ~Quote() = default;

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
    void from_json(const json & j, quicktype::Orders & x);
    void to_json(json & j, const quicktype::Orders & x);

    void from_json(const json & j, quicktype::BuyProperties & x);
    void to_json(json & j, const quicktype::BuyProperties & x);

    void from_json(const json & j, quicktype::Buy & x);
    void to_json(json & j, const quicktype::Buy & x);

    void from_json(const json & j, quicktype::NseInfy & x);
    void to_json(json & j, const quicktype::NseInfy & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::BuyClass & x);
    void to_json(json & j, const quicktype::BuyClass & x);

    void from_json(const json & j, quicktype::DepthProperties & x);
    void to_json(json & j, const quicktype::DepthProperties & x);

    void from_json(const json & j, quicktype::Depth & x);
    void to_json(json & j, const quicktype::Depth & x);

    void from_json(const json & j, quicktype::LastTradeTime & x);
    void to_json(json & j, const quicktype::LastTradeTime & x);

    void from_json(const json & j, quicktype::NseInfyProperties & x);
    void to_json(json & j, const quicktype::NseInfyProperties & x);

    void from_json(const json & j, quicktype::NseInfyClass & x);
    void to_json(json & j, const quicktype::NseInfyClass & x);

    void from_json(const json & j, quicktype::OhlcProperties & x);
    void to_json(json & j, const quicktype::OhlcProperties & x);

    void from_json(const json & j, quicktype::Ohlc & x);
    void to_json(json & j, const quicktype::Ohlc & x);

    void from_json(const json & j, quicktype::QuoteProperties & x);
    void to_json(json & j, const quicktype::QuoteProperties & x);

    void from_json(const json & j, quicktype::QuoteClass & x);
    void to_json(json & j, const quicktype::QuoteClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::Quote & x);
    void to_json(json & j, const quicktype::Quote & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::Orders& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::Orders & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::BuyProperties& x) {
        x.set_orders(j.at("orders").get<quicktype::Orders>());
        x.set_price(j.at("price").get<quicktype::Orders>());
        x.set_quantity(j.at("quantity").get<quicktype::Orders>());
    }

    inline void to_json(json & j, const quicktype::BuyProperties & x) {
        j = json::object();
        j["orders"] = x.get_orders();
        j["price"] = x.get_price();
        j["quantity"] = x.get_quantity();
    }

    inline void from_json(const json & j, quicktype::Buy& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::BuyProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Buy & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::NseInfy& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::NseInfy & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_nse_infy(j.at("NSE:INFY").get<quicktype::NseInfy>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["NSE:INFY"] = x.get_nse_infy();
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

    inline void from_json(const json & j, quicktype::BuyClass& x) {
        x.set_items(j.at("items").get<quicktype::NseInfy>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::BuyClass & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DepthProperties& x) {
        x.set_buy(j.at("buy").get<quicktype::BuyClass>());
        x.set_sell(j.at("sell").get<quicktype::BuyClass>());
    }

    inline void to_json(json & j, const quicktype::DepthProperties & x) {
        j = json::object();
        j["buy"] = x.get_buy();
        j["sell"] = x.get_sell();
    }

    inline void from_json(const json & j, quicktype::Depth& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::DepthProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Depth & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::LastTradeTime& x) {
        x.set_format(j.at("format").get<std::string>());
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::LastTradeTime & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::NseInfyProperties& x) {
        x.set_average_price(j.at("average_price").get<quicktype::Orders>());
        x.set_buy_quantity(j.at("buy_quantity").get<quicktype::Orders>());
        x.set_depth(j.at("depth").get<quicktype::NseInfy>());
        x.set_instrument_token(j.at("instrument_token").get<quicktype::Orders>());
        x.set_last_price(j.at("last_price").get<quicktype::Orders>());
        x.set_last_quantity(j.at("last_quantity").get<quicktype::Orders>());
        x.set_last_trade_time(j.at("last_trade_time").get<quicktype::LastTradeTime>());
        x.set_lower_circuit_limit(j.at("lower_circuit_limit").get<quicktype::Orders>());
        x.set_net_change(j.at("net_change").get<quicktype::Orders>());
        x.set_ohlc(j.at("ohlc").get<quicktype::NseInfy>());
        x.set_oi(j.at("oi").get<quicktype::Orders>());
        x.set_oi_day_high(j.at("oi_day_high").get<quicktype::Orders>());
        x.set_oi_day_low(j.at("oi_day_low").get<quicktype::Orders>());
        x.set_sell_quantity(j.at("sell_quantity").get<quicktype::Orders>());
        x.set_timestamp(j.at("timestamp").get<quicktype::LastTradeTime>());
        x.set_upper_circuit_limit(j.at("upper_circuit_limit").get<quicktype::Orders>());
        x.set_volume(j.at("volume").get<quicktype::Orders>());
    }

    inline void to_json(json & j, const quicktype::NseInfyProperties & x) {
        j = json::object();
        j["average_price"] = x.get_average_price();
        j["buy_quantity"] = x.get_buy_quantity();
        j["depth"] = x.get_depth();
        j["instrument_token"] = x.get_instrument_token();
        j["last_price"] = x.get_last_price();
        j["last_quantity"] = x.get_last_quantity();
        j["last_trade_time"] = x.get_last_trade_time();
        j["lower_circuit_limit"] = x.get_lower_circuit_limit();
        j["net_change"] = x.get_net_change();
        j["ohlc"] = x.get_ohlc();
        j["oi"] = x.get_oi();
        j["oi_day_high"] = x.get_oi_day_high();
        j["oi_day_low"] = x.get_oi_day_low();
        j["sell_quantity"] = x.get_sell_quantity();
        j["timestamp"] = x.get_timestamp();
        j["upper_circuit_limit"] = x.get_upper_circuit_limit();
        j["volume"] = x.get_volume();
    }

    inline void from_json(const json & j, quicktype::NseInfyClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::NseInfyProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::NseInfyClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::OhlcProperties& x) {
        x.set_close(j.at("close").get<quicktype::Orders>());
        x.set_high(j.at("high").get<quicktype::Orders>());
        x.set_low(j.at("low").get<quicktype::Orders>());
        x.set_open(j.at("open").get<quicktype::Orders>());
    }

    inline void to_json(json & j, const quicktype::OhlcProperties & x) {
        j = json::object();
        j["close"] = x.get_close();
        j["high"] = x.get_high();
        j["low"] = x.get_low();
        j["open"] = x.get_open();
    }

    inline void from_json(const json & j, quicktype::Ohlc& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::OhlcProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Ohlc & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::QuoteProperties& x) {
        x.set_data(j.at("data").get<quicktype::NseInfy>());
        x.set_status(j.at("status").get<quicktype::Orders>());
    }

    inline void to_json(json & j, const quicktype::QuoteProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::QuoteClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::QuoteProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::QuoteClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_buy(j.at("Buy").get<quicktype::Buy>());
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_depth(j.at("Depth").get<quicktype::Depth>());
        x.set_nse_infy(j.at("NseInfy").get<quicktype::NseInfyClass>());
        x.set_ohlc(j.at("Ohlc").get<quicktype::Ohlc>());
        x.set_quote(j.at("Quote").get<quicktype::QuoteClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Buy"] = x.get_buy();
        j["Data"] = x.get_data();
        j["Depth"] = x.get_depth();
        j["NseInfy"] = x.get_nse_infy();
        j["Ohlc"] = x.get_ohlc();
        j["Quote"] = x.get_quote();
    }

    inline void from_json(const json & j, quicktype::Quote& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::Quote & x) {
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
