//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     BasketMargins data = nlohmann::json::parse(jsonString);

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

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    enum class Type : int { INTEGER, NUMBER, STRING };

    class Status {
        public:
        Status() = default;
        virtual ~Status() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class BasketMarginsProperties {
        public:
        BasketMarginsProperties() = default;
        virtual ~BasketMarginsProperties() = default;

        private:
        Data data;
        Status status;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const Status & get_status() const { return status; }
        Status & get_mutable_status() { return status; }
        void set_status(const Status & value) { this->status = value; }
    };

    class BasketMarginsClass {
        public:
        BasketMarginsClass() = default;
        virtual ~BasketMarginsClass() = default;

        private:
        bool additional_properties;
        BasketMarginsProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const BasketMarginsProperties & get_properties() const { return properties; }
        BasketMarginsProperties & get_mutable_properties() { return properties; }
        void set_properties(const BasketMarginsProperties & value) { this->properties = value; }

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

    class Orders {
        public:
        Orders() = default;
        virtual ~Orders() = default;

        private:
        Data items;
        std::string type;

        public:
        const Data & get_items() const { return items; }
        Data & get_mutable_items() { return items; }
        void set_items(const Data & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class DataProperties {
        public:
        DataProperties() = default;
        virtual ~DataProperties() = default;

        private:
        Data data_properties_final;
        Data initial;
        Orders orders;

        public:
        const Data & get_data_properties_final() const { return data_properties_final; }
        Data & get_mutable_data_properties_final() { return data_properties_final; }
        void set_data_properties_final(const Data & value) { this->data_properties_final = value; }

        const Data & get_initial() const { return initial; }
        Data & get_mutable_initial() { return initial; }
        void set_initial(const Data & value) { this->initial = value; }

        const Orders & get_orders() const { return orders; }
        Orders & get_mutable_orders() { return orders; }
        void set_orders(const Orders & value) { this->orders = value; }
    };

    class DataClass {
        public:
        DataClass() = default;
        virtual ~DataClass() = default;

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

    class FinalProperties {
        public:
        FinalProperties() = default;
        virtual ~FinalProperties() = default;

        private:
        Status additional;
        Status bo;
        Status cash;
        Status exchange;
        Status exposure;
        Status option_premium;
        Data pnl;
        Status span;
        Status total;
        Status tradingsymbol;
        Status type;
        Status var;

        public:
        const Status & get_additional() const { return additional; }
        Status & get_mutable_additional() { return additional; }
        void set_additional(const Status & value) { this->additional = value; }

        const Status & get_bo() const { return bo; }
        Status & get_mutable_bo() { return bo; }
        void set_bo(const Status & value) { this->bo = value; }

        const Status & get_cash() const { return cash; }
        Status & get_mutable_cash() { return cash; }
        void set_cash(const Status & value) { this->cash = value; }

        const Status & get_exchange() const { return exchange; }
        Status & get_mutable_exchange() { return exchange; }
        void set_exchange(const Status & value) { this->exchange = value; }

        const Status & get_exposure() const { return exposure; }
        Status & get_mutable_exposure() { return exposure; }
        void set_exposure(const Status & value) { this->exposure = value; }

        const Status & get_option_premium() const { return option_premium; }
        Status & get_mutable_option_premium() { return option_premium; }
        void set_option_premium(const Status & value) { this->option_premium = value; }

        const Data & get_pnl() const { return pnl; }
        Data & get_mutable_pnl() { return pnl; }
        void set_pnl(const Data & value) { this->pnl = value; }

        const Status & get_span() const { return span; }
        Status & get_mutable_span() { return span; }
        void set_span(const Status & value) { this->span = value; }

        const Status & get_total() const { return total; }
        Status & get_mutable_total() { return total; }
        void set_total(const Status & value) { this->total = value; }

        const Status & get_tradingsymbol() const { return tradingsymbol; }
        Status & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const Status & value) { this->tradingsymbol = value; }

        const Status & get_type() const { return type; }
        Status & get_mutable_type() { return type; }
        void set_type(const Status & value) { this->type = value; }

        const Status & get_var() const { return var; }
        Status & get_mutable_var() { return var; }
        void set_var(const Status & value) { this->var = value; }
    };

    class Final {
        public:
        Final() = default;
        virtual ~Final() = default;

        private:
        bool additional_properties;
        FinalProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const FinalProperties & get_properties() const { return properties; }
        FinalProperties & get_mutable_properties() { return properties; }
        void set_properties(const FinalProperties & value) { this->properties = value; }

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

    class PnlProperties {
        public:
        PnlProperties() = default;
        virtual ~PnlProperties() = default;

        private:
        Status realised;
        Status unrealised;

        public:
        const Status & get_realised() const { return realised; }
        Status & get_mutable_realised() { return realised; }
        void set_realised(const Status & value) { this->realised = value; }

        const Status & get_unrealised() const { return unrealised; }
        Status & get_mutable_unrealised() { return unrealised; }
        void set_unrealised(const Status & value) { this->unrealised = value; }
    };

    class Pnl {
        public:
        Pnl() = default;
        virtual ~Pnl() = default;

        private:
        bool additional_properties;
        PnlProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const PnlProperties & get_properties() const { return properties; }
        PnlProperties & get_mutable_properties() { return properties; }
        void set_properties(const PnlProperties & value) { this->properties = value; }

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
        BasketMarginsClass basket_margins;
        DataClass data;
        Final definitions_final;
        Pnl pnl;

        public:
        const BasketMarginsClass & get_basket_margins() const { return basket_margins; }
        BasketMarginsClass & get_mutable_basket_margins() { return basket_margins; }
        void set_basket_margins(const BasketMarginsClass & value) { this->basket_margins = value; }

        const DataClass & get_data() const { return data; }
        DataClass & get_mutable_data() { return data; }
        void set_data(const DataClass & value) { this->data = value; }

        const Final & get_definitions_final() const { return definitions_final; }
        Final & get_mutable_definitions_final() { return definitions_final; }
        void set_definitions_final(const Final & value) { this->definitions_final = value; }

        const Pnl & get_pnl() const { return pnl; }
        Pnl & get_mutable_pnl() { return pnl; }
        void set_pnl(const Pnl & value) { this->pnl = value; }
    };

    class BasketMargins {
        public:
        BasketMargins() = default;
        virtual ~BasketMargins() = default;

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
    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::Status & x);
    void to_json(json & j, const quicktype::Status & x);

    void from_json(const json & j, quicktype::BasketMarginsProperties & x);
    void to_json(json & j, const quicktype::BasketMarginsProperties & x);

    void from_json(const json & j, quicktype::BasketMarginsClass & x);
    void to_json(json & j, const quicktype::BasketMarginsClass & x);

    void from_json(const json & j, quicktype::Orders & x);
    void to_json(json & j, const quicktype::Orders & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::DataClass & x);
    void to_json(json & j, const quicktype::DataClass & x);

    void from_json(const json & j, quicktype::FinalProperties & x);
    void to_json(json & j, const quicktype::FinalProperties & x);

    void from_json(const json & j, quicktype::Final & x);
    void to_json(json & j, const quicktype::Final & x);

    void from_json(const json & j, quicktype::PnlProperties & x);
    void to_json(json & j, const quicktype::PnlProperties & x);

    void from_json(const json & j, quicktype::Pnl & x);
    void to_json(json & j, const quicktype::Pnl & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::BasketMargins & x);
    void to_json(json & j, const quicktype::BasketMargins & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::Data& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Data & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::Status& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::Status & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::BasketMarginsProperties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
        x.set_status(j.at("status").get<quicktype::Status>());
    }

    inline void to_json(json & j, const quicktype::BasketMarginsProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::BasketMarginsClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::BasketMarginsProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::BasketMarginsClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Orders& x) {
        x.set_items(j.at("items").get<quicktype::Data>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Orders & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_data_properties_final(j.at("final").get<quicktype::Data>());
        x.set_initial(j.at("initial").get<quicktype::Data>());
        x.set_orders(j.at("orders").get<quicktype::Orders>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["final"] = x.get_data_properties_final();
        j["initial"] = x.get_initial();
        j["orders"] = x.get_orders();
    }

    inline void from_json(const json & j, quicktype::DataClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::DataProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::DataClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::FinalProperties& x) {
        x.set_additional(j.at("additional").get<quicktype::Status>());
        x.set_bo(j.at("bo").get<quicktype::Status>());
        x.set_cash(j.at("cash").get<quicktype::Status>());
        x.set_exchange(j.at("exchange").get<quicktype::Status>());
        x.set_exposure(j.at("exposure").get<quicktype::Status>());
        x.set_option_premium(j.at("option_premium").get<quicktype::Status>());
        x.set_pnl(j.at("pnl").get<quicktype::Data>());
        x.set_span(j.at("span").get<quicktype::Status>());
        x.set_total(j.at("total").get<quicktype::Status>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::Status>());
        x.set_type(j.at("type").get<quicktype::Status>());
        x.set_var(j.at("var").get<quicktype::Status>());
    }

    inline void to_json(json & j, const quicktype::FinalProperties & x) {
        j = json::object();
        j["additional"] = x.get_additional();
        j["bo"] = x.get_bo();
        j["cash"] = x.get_cash();
        j["exchange"] = x.get_exchange();
        j["exposure"] = x.get_exposure();
        j["option_premium"] = x.get_option_premium();
        j["pnl"] = x.get_pnl();
        j["span"] = x.get_span();
        j["total"] = x.get_total();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["type"] = x.get_type();
        j["var"] = x.get_var();
    }

    inline void from_json(const json & j, quicktype::Final& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::FinalProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Final & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::PnlProperties& x) {
        x.set_realised(j.at("realised").get<quicktype::Status>());
        x.set_unrealised(j.at("unrealised").get<quicktype::Status>());
    }

    inline void to_json(json & j, const quicktype::PnlProperties & x) {
        j = json::object();
        j["realised"] = x.get_realised();
        j["unrealised"] = x.get_unrealised();
    }

    inline void from_json(const json & j, quicktype::Pnl& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::PnlProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Pnl & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_basket_margins(j.at("BasketMargins").get<quicktype::BasketMarginsClass>());
        x.set_data(j.at("Data").get<quicktype::DataClass>());
        x.set_definitions_final(j.at("Final").get<quicktype::Final>());
        x.set_pnl(j.at("Pnl").get<quicktype::Pnl>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["BasketMargins"] = x.get_basket_margins();
        j["Data"] = x.get_data();
        j["Final"] = x.get_definitions_final();
        j["Pnl"] = x.get_pnl();
    }

    inline void from_json(const json & j, quicktype::BasketMargins& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::BasketMargins & x) {
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
