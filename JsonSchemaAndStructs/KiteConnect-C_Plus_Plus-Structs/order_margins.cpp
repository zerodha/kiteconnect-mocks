//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     OrderMargins data = nlohmann::json::parse(jsonString);

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

    class Additional {
        public:
        Additional() = default;
        virtual ~Additional() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class Pnl {
        public:
        Pnl() = default;
        virtual ~Pnl() = default;

        private:
        std::string ref;

        public:
        const std::string & get_ref() const { return ref; }
        std::string & get_mutable_ref() { return ref; }
        void set_ref(const std::string & value) { this->ref = value; }
    };

    class DatumProperties {
        public:
        DatumProperties() = default;
        virtual ~DatumProperties() = default;

        private:
        Additional additional;
        Additional bo;
        Additional cash;
        Additional exchange;
        Additional exposure;
        Additional option_premium;
        Pnl pnl;
        Additional span;
        Additional total;
        Additional tradingsymbol;
        Additional type;
        Additional var;

        public:
        const Additional & get_additional() const { return additional; }
        Additional & get_mutable_additional() { return additional; }
        void set_additional(const Additional & value) { this->additional = value; }

        const Additional & get_bo() const { return bo; }
        Additional & get_mutable_bo() { return bo; }
        void set_bo(const Additional & value) { this->bo = value; }

        const Additional & get_cash() const { return cash; }
        Additional & get_mutable_cash() { return cash; }
        void set_cash(const Additional & value) { this->cash = value; }

        const Additional & get_exchange() const { return exchange; }
        Additional & get_mutable_exchange() { return exchange; }
        void set_exchange(const Additional & value) { this->exchange = value; }

        const Additional & get_exposure() const { return exposure; }
        Additional & get_mutable_exposure() { return exposure; }
        void set_exposure(const Additional & value) { this->exposure = value; }

        const Additional & get_option_premium() const { return option_premium; }
        Additional & get_mutable_option_premium() { return option_premium; }
        void set_option_premium(const Additional & value) { this->option_premium = value; }

        const Pnl & get_pnl() const { return pnl; }
        Pnl & get_mutable_pnl() { return pnl; }
        void set_pnl(const Pnl & value) { this->pnl = value; }

        const Additional & get_span() const { return span; }
        Additional & get_mutable_span() { return span; }
        void set_span(const Additional & value) { this->span = value; }

        const Additional & get_total() const { return total; }
        Additional & get_mutable_total() { return total; }
        void set_total(const Additional & value) { this->total = value; }

        const Additional & get_tradingsymbol() const { return tradingsymbol; }
        Additional & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const Additional & value) { this->tradingsymbol = value; }

        const Additional & get_type() const { return type; }
        Additional & get_mutable_type() { return type; }
        void set_type(const Additional & value) { this->type = value; }

        const Additional & get_var() const { return var; }
        Additional & get_mutable_var() { return var; }
        void set_var(const Additional & value) { this->var = value; }
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

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        Pnl items;
        std::string type;

        public:
        const Pnl & get_items() const { return items; }
        Pnl & get_mutable_items() { return items; }
        void set_items(const Pnl & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class OrderMarginsProperties {
        public:
        OrderMarginsProperties() = default;
        virtual ~OrderMarginsProperties() = default;

        private:
        Data data;
        Additional status;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const Additional & get_status() const { return status; }
        Additional & get_mutable_status() { return status; }
        void set_status(const Additional & value) { this->status = value; }
    };

    class OrderMarginsClass {
        public:
        OrderMarginsClass() = default;
        virtual ~OrderMarginsClass() = default;

        private:
        bool additional_properties;
        OrderMarginsProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const OrderMarginsProperties & get_properties() const { return properties; }
        OrderMarginsProperties & get_mutable_properties() { return properties; }
        void set_properties(const OrderMarginsProperties & value) { this->properties = value; }

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
        Additional realised;
        Additional unrealised;

        public:
        const Additional & get_realised() const { return realised; }
        Additional & get_mutable_realised() { return realised; }
        void set_realised(const Additional & value) { this->realised = value; }

        const Additional & get_unrealised() const { return unrealised; }
        Additional & get_mutable_unrealised() { return unrealised; }
        void set_unrealised(const Additional & value) { this->unrealised = value; }
    };

    class PnlClass {
        public:
        PnlClass() = default;
        virtual ~PnlClass() = default;

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
        Datum datum;
        OrderMarginsClass order_margins;
        PnlClass pnl;

        public:
        const Datum & get_datum() const { return datum; }
        Datum & get_mutable_datum() { return datum; }
        void set_datum(const Datum & value) { this->datum = value; }

        const OrderMarginsClass & get_order_margins() const { return order_margins; }
        OrderMarginsClass & get_mutable_order_margins() { return order_margins; }
        void set_order_margins(const OrderMarginsClass & value) { this->order_margins = value; }

        const PnlClass & get_pnl() const { return pnl; }
        PnlClass & get_mutable_pnl() { return pnl; }
        void set_pnl(const PnlClass & value) { this->pnl = value; }
    };

    class OrderMargins {
        public:
        OrderMargins() = default;
        virtual ~OrderMargins() = default;

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
    void from_json(const json & j, quicktype::Additional & x);
    void to_json(json & j, const quicktype::Additional & x);

    void from_json(const json & j, quicktype::Pnl & x);
    void to_json(json & j, const quicktype::Pnl & x);

    void from_json(const json & j, quicktype::DatumProperties & x);
    void to_json(json & j, const quicktype::DatumProperties & x);

    void from_json(const json & j, quicktype::Datum & x);
    void to_json(json & j, const quicktype::Datum & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::OrderMarginsProperties & x);
    void to_json(json & j, const quicktype::OrderMarginsProperties & x);

    void from_json(const json & j, quicktype::OrderMarginsClass & x);
    void to_json(json & j, const quicktype::OrderMarginsClass & x);

    void from_json(const json & j, quicktype::PnlProperties & x);
    void to_json(json & j, const quicktype::PnlProperties & x);

    void from_json(const json & j, quicktype::PnlClass & x);
    void to_json(json & j, const quicktype::PnlClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::OrderMargins & x);
    void to_json(json & j, const quicktype::OrderMargins & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::Additional& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::Additional & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Pnl& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Pnl & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::DatumProperties& x) {
        x.set_additional(j.at("additional").get<quicktype::Additional>());
        x.set_bo(j.at("bo").get<quicktype::Additional>());
        x.set_cash(j.at("cash").get<quicktype::Additional>());
        x.set_exchange(j.at("exchange").get<quicktype::Additional>());
        x.set_exposure(j.at("exposure").get<quicktype::Additional>());
        x.set_option_premium(j.at("option_premium").get<quicktype::Additional>());
        x.set_pnl(j.at("pnl").get<quicktype::Pnl>());
        x.set_span(j.at("span").get<quicktype::Additional>());
        x.set_total(j.at("total").get<quicktype::Additional>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::Additional>());
        x.set_type(j.at("type").get<quicktype::Additional>());
        x.set_var(j.at("var").get<quicktype::Additional>());
    }

    inline void to_json(json & j, const quicktype::DatumProperties & x) {
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

    inline void from_json(const json & j, quicktype::Data& x) {
        x.set_items(j.at("items").get<quicktype::Pnl>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Data & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::OrderMarginsProperties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
        x.set_status(j.at("status").get<quicktype::Additional>());
    }

    inline void to_json(json & j, const quicktype::OrderMarginsProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::OrderMarginsClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::OrderMarginsProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::OrderMarginsClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::PnlProperties& x) {
        x.set_realised(j.at("realised").get<quicktype::Additional>());
        x.set_unrealised(j.at("unrealised").get<quicktype::Additional>());
    }

    inline void to_json(json & j, const quicktype::PnlProperties & x) {
        j = json::object();
        j["realised"] = x.get_realised();
        j["unrealised"] = x.get_unrealised();
    }

    inline void from_json(const json & j, quicktype::PnlClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::PnlProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::PnlClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_datum(j.at("Datum").get<quicktype::Datum>());
        x.set_order_margins(j.at("OrderMargins").get<quicktype::OrderMarginsClass>());
        x.set_pnl(j.at("Pnl").get<quicktype::PnlClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Datum"] = x.get_datum();
        j["OrderMargins"] = x.get_order_margins();
        j["Pnl"] = x.get_pnl();
    }

    inline void from_json(const json & j, quicktype::OrderMargins& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::OrderMargins & x) {
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
