//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     MfSips data = nlohmann::json::parse(jsonString);

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

    enum class Type : int { INTEGER, NUMBER, STRING };

    class CompletedInstalments {
        public:
        CompletedInstalments() = default;
        virtual ~CompletedInstalments() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class Created {
        public:
        Created() = default;
        virtual ~Created() = default;

        private:
        std::shared_ptr<std::string> format;
        std::string type;

        public:
        std::shared_ptr<std::string> get_format() const { return format; }
        void set_format(std::shared_ptr<std::string> value) { this->format = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class SipRegNum {
        public:
        SipRegNum() = default;
        virtual ~SipRegNum() = default;

        private:
        std::vector<Created> any_of;

        public:
        const std::vector<Created> & get_any_of() const { return any_of; }
        std::vector<Created> & get_mutable_any_of() { return any_of; }
        void set_any_of(const std::vector<Created> & value) { this->any_of = value; }
    };

    class StepUp {
        public:
        StepUp() = default;
        virtual ~StepUp() = default;

        private:
        CompletedInstalments additional_properties;
        std::string type;

        public:
        const CompletedInstalments & get_additional_properties() const { return additional_properties; }
        CompletedInstalments & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const CompletedInstalments & value) { this->additional_properties = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class DatumProperties {
        public:
        DatumProperties() = default;
        virtual ~DatumProperties() = default;

        private:
        CompletedInstalments completed_instalments;
        Created created;
        CompletedInstalments dividend_type;
        CompletedInstalments frequency;
        CompletedInstalments fund;
        CompletedInstalments instalment_amount;
        CompletedInstalments instalment_day;
        CompletedInstalments instalments;
        Created last_instalment;
        Created next_instalment;
        CompletedInstalments pending_instalments;
        CompletedInstalments sip_id;
        SipRegNum sip_reg_num;
        CompletedInstalments sip_type;
        CompletedInstalments status;
        StepUp step_up;
        CompletedInstalments tag;
        CompletedInstalments tradingsymbol;
        CompletedInstalments transaction_type;
        CompletedInstalments trigger_price;

        public:
        const CompletedInstalments & get_completed_instalments() const { return completed_instalments; }
        CompletedInstalments & get_mutable_completed_instalments() { return completed_instalments; }
        void set_completed_instalments(const CompletedInstalments & value) { this->completed_instalments = value; }

        const Created & get_created() const { return created; }
        Created & get_mutable_created() { return created; }
        void set_created(const Created & value) { this->created = value; }

        const CompletedInstalments & get_dividend_type() const { return dividend_type; }
        CompletedInstalments & get_mutable_dividend_type() { return dividend_type; }
        void set_dividend_type(const CompletedInstalments & value) { this->dividend_type = value; }

        const CompletedInstalments & get_frequency() const { return frequency; }
        CompletedInstalments & get_mutable_frequency() { return frequency; }
        void set_frequency(const CompletedInstalments & value) { this->frequency = value; }

        const CompletedInstalments & get_fund() const { return fund; }
        CompletedInstalments & get_mutable_fund() { return fund; }
        void set_fund(const CompletedInstalments & value) { this->fund = value; }

        const CompletedInstalments & get_instalment_amount() const { return instalment_amount; }
        CompletedInstalments & get_mutable_instalment_amount() { return instalment_amount; }
        void set_instalment_amount(const CompletedInstalments & value) { this->instalment_amount = value; }

        const CompletedInstalments & get_instalment_day() const { return instalment_day; }
        CompletedInstalments & get_mutable_instalment_day() { return instalment_day; }
        void set_instalment_day(const CompletedInstalments & value) { this->instalment_day = value; }

        const CompletedInstalments & get_instalments() const { return instalments; }
        CompletedInstalments & get_mutable_instalments() { return instalments; }
        void set_instalments(const CompletedInstalments & value) { this->instalments = value; }

        const Created & get_last_instalment() const { return last_instalment; }
        Created & get_mutable_last_instalment() { return last_instalment; }
        void set_last_instalment(const Created & value) { this->last_instalment = value; }

        const Created & get_next_instalment() const { return next_instalment; }
        Created & get_mutable_next_instalment() { return next_instalment; }
        void set_next_instalment(const Created & value) { this->next_instalment = value; }

        const CompletedInstalments & get_pending_instalments() const { return pending_instalments; }
        CompletedInstalments & get_mutable_pending_instalments() { return pending_instalments; }
        void set_pending_instalments(const CompletedInstalments & value) { this->pending_instalments = value; }

        const CompletedInstalments & get_sip_id() const { return sip_id; }
        CompletedInstalments & get_mutable_sip_id() { return sip_id; }
        void set_sip_id(const CompletedInstalments & value) { this->sip_id = value; }

        const SipRegNum & get_sip_reg_num() const { return sip_reg_num; }
        SipRegNum & get_mutable_sip_reg_num() { return sip_reg_num; }
        void set_sip_reg_num(const SipRegNum & value) { this->sip_reg_num = value; }

        const CompletedInstalments & get_sip_type() const { return sip_type; }
        CompletedInstalments & get_mutable_sip_type() { return sip_type; }
        void set_sip_type(const CompletedInstalments & value) { this->sip_type = value; }

        const CompletedInstalments & get_status() const { return status; }
        CompletedInstalments & get_mutable_status() { return status; }
        void set_status(const CompletedInstalments & value) { this->status = value; }

        const StepUp & get_step_up() const { return step_up; }
        StepUp & get_mutable_step_up() { return step_up; }
        void set_step_up(const StepUp & value) { this->step_up = value; }

        const CompletedInstalments & get_tag() const { return tag; }
        CompletedInstalments & get_mutable_tag() { return tag; }
        void set_tag(const CompletedInstalments & value) { this->tag = value; }

        const CompletedInstalments & get_tradingsymbol() const { return tradingsymbol; }
        CompletedInstalments & get_mutable_tradingsymbol() { return tradingsymbol; }
        void set_tradingsymbol(const CompletedInstalments & value) { this->tradingsymbol = value; }

        const CompletedInstalments & get_transaction_type() const { return transaction_type; }
        CompletedInstalments & get_mutable_transaction_type() { return transaction_type; }
        void set_transaction_type(const CompletedInstalments & value) { this->transaction_type = value; }

        const CompletedInstalments & get_trigger_price() const { return trigger_price; }
        CompletedInstalments & get_mutable_trigger_price() { return trigger_price; }
        void set_trigger_price(const CompletedInstalments & value) { this->trigger_price = value; }
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

    class MfSipsProperties {
        public:
        MfSipsProperties() = default;
        virtual ~MfSipsProperties() = default;

        private:
        Data data;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }
    };

    class MfSipsClass {
        public:
        MfSipsClass() = default;
        virtual ~MfSipsClass() = default;

        private:
        bool additional_properties;
        MfSipsProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const MfSipsProperties & get_properties() const { return properties; }
        MfSipsProperties & get_mutable_properties() { return properties; }
        void set_properties(const MfSipsProperties & value) { this->properties = value; }

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
        MfSipsClass mf_sips;

        public:
        const Datum & get_datum() const { return datum; }
        Datum & get_mutable_datum() { return datum; }
        void set_datum(const Datum & value) { this->datum = value; }

        const MfSipsClass & get_mf_sips() const { return mf_sips; }
        MfSipsClass & get_mutable_mf_sips() { return mf_sips; }
        void set_mf_sips(const MfSipsClass & value) { this->mf_sips = value; }
    };

    class MfSips {
        public:
        MfSips() = default;
        virtual ~MfSips() = default;

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
    void from_json(const json & j, quicktype::CompletedInstalments & x);
    void to_json(json & j, const quicktype::CompletedInstalments & x);

    void from_json(const json & j, quicktype::Created & x);
    void to_json(json & j, const quicktype::Created & x);

    void from_json(const json & j, quicktype::SipRegNum & x);
    void to_json(json & j, const quicktype::SipRegNum & x);

    void from_json(const json & j, quicktype::StepUp & x);
    void to_json(json & j, const quicktype::StepUp & x);

    void from_json(const json & j, quicktype::DatumProperties & x);
    void to_json(json & j, const quicktype::DatumProperties & x);

    void from_json(const json & j, quicktype::Datum & x);
    void to_json(json & j, const quicktype::Datum & x);

    void from_json(const json & j, quicktype::Items & x);
    void to_json(json & j, const quicktype::Items & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::MfSipsProperties & x);
    void to_json(json & j, const quicktype::MfSipsProperties & x);

    void from_json(const json & j, quicktype::MfSipsClass & x);
    void to_json(json & j, const quicktype::MfSipsClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::MfSips & x);
    void to_json(json & j, const quicktype::MfSips & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::CompletedInstalments& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::CompletedInstalments & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Created& x) {
        x.set_format(quicktype::get_optional<std::string>(j, "format"));
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Created & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::SipRegNum& x) {
        x.set_any_of(j.at("anyOf").get<std::vector<quicktype::Created>>());
    }

    inline void to_json(json & j, const quicktype::SipRegNum & x) {
        j = json::object();
        j["anyOf"] = x.get_any_of();
    }

    inline void from_json(const json & j, quicktype::StepUp& x) {
        x.set_additional_properties(j.at("additionalProperties").get<quicktype::CompletedInstalments>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::StepUp & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::DatumProperties& x) {
        x.set_completed_instalments(j.at("completed_instalments").get<quicktype::CompletedInstalments>());
        x.set_created(j.at("created").get<quicktype::Created>());
        x.set_dividend_type(j.at("dividend_type").get<quicktype::CompletedInstalments>());
        x.set_frequency(j.at("frequency").get<quicktype::CompletedInstalments>());
        x.set_fund(j.at("fund").get<quicktype::CompletedInstalments>());
        x.set_instalment_amount(j.at("instalment_amount").get<quicktype::CompletedInstalments>());
        x.set_instalment_day(j.at("instalment_day").get<quicktype::CompletedInstalments>());
        x.set_instalments(j.at("instalments").get<quicktype::CompletedInstalments>());
        x.set_last_instalment(j.at("last_instalment").get<quicktype::Created>());
        x.set_next_instalment(j.at("next_instalment").get<quicktype::Created>());
        x.set_pending_instalments(j.at("pending_instalments").get<quicktype::CompletedInstalments>());
        x.set_sip_id(j.at("sip_id").get<quicktype::CompletedInstalments>());
        x.set_sip_reg_num(j.at("sip_reg_num").get<quicktype::SipRegNum>());
        x.set_sip_type(j.at("sip_type").get<quicktype::CompletedInstalments>());
        x.set_status(j.at("status").get<quicktype::CompletedInstalments>());
        x.set_step_up(j.at("step_up").get<quicktype::StepUp>());
        x.set_tag(j.at("tag").get<quicktype::CompletedInstalments>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::CompletedInstalments>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::CompletedInstalments>());
        x.set_trigger_price(j.at("trigger_price").get<quicktype::CompletedInstalments>());
    }

    inline void to_json(json & j, const quicktype::DatumProperties & x) {
        j = json::object();
        j["completed_instalments"] = x.get_completed_instalments();
        j["created"] = x.get_created();
        j["dividend_type"] = x.get_dividend_type();
        j["frequency"] = x.get_frequency();
        j["fund"] = x.get_fund();
        j["instalment_amount"] = x.get_instalment_amount();
        j["instalment_day"] = x.get_instalment_day();
        j["instalments"] = x.get_instalments();
        j["last_instalment"] = x.get_last_instalment();
        j["next_instalment"] = x.get_next_instalment();
        j["pending_instalments"] = x.get_pending_instalments();
        j["sip_id"] = x.get_sip_id();
        j["sip_reg_num"] = x.get_sip_reg_num();
        j["sip_type"] = x.get_sip_type();
        j["status"] = x.get_status();
        j["step_up"] = x.get_step_up();
        j["tag"] = x.get_tag();
        j["tradingsymbol"] = x.get_tradingsymbol();
        j["transaction_type"] = x.get_transaction_type();
        j["trigger_price"] = x.get_trigger_price();
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

    inline void from_json(const json & j, quicktype::MfSipsProperties& x) {
        x.set_data(j.at("data").get<quicktype::Data>());
    }

    inline void to_json(json & j, const quicktype::MfSipsProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
    }

    inline void from_json(const json & j, quicktype::MfSipsClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::MfSipsProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MfSipsClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_datum(j.at("Datum").get<quicktype::Datum>());
        x.set_mf_sips(j.at("MFSips").get<quicktype::MfSipsClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Datum"] = x.get_datum();
        j["MFSips"] = x.get_mf_sips();
    }

    inline void from_json(const json & j, quicktype::MfSips& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::MfSips & x) {
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
