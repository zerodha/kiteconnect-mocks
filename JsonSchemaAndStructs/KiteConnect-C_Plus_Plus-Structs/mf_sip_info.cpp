//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     MfSipInfo data = nlohmann::json::parse(jsonString);

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

    class StepUp {
        public:
        StepUp() = default;
        virtual ~StepUp() = default;

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
        CompletedInstalments completed_instalments;
        Created created;
        CompletedInstalments dividend_type;
        CompletedInstalments frequency;
        CompletedInstalments fund;
        CompletedInstalments fund_source;
        CompletedInstalments instalment_amount;
        CompletedInstalments instalment_day;
        CompletedInstalments instalments;
        Created last_instalment;
        Created next_instalment;
        CompletedInstalments pending_instalments;
        CompletedInstalments sip_id;
        CompletedInstalments sip_reg_num;
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

        const CompletedInstalments & get_fund_source() const { return fund_source; }
        CompletedInstalments & get_mutable_fund_source() { return fund_source; }
        void set_fund_source(const CompletedInstalments & value) { this->fund_source = value; }

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

        const CompletedInstalments & get_sip_reg_num() const { return sip_reg_num; }
        CompletedInstalments & get_mutable_sip_reg_num() { return sip_reg_num; }
        void set_sip_reg_num(const CompletedInstalments & value) { this->sip_reg_num = value; }

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

    class MfsipInfoProperties {
        public:
        MfsipInfoProperties() = default;
        virtual ~MfsipInfoProperties() = default;

        private:
        StepUp data;
        CompletedInstalments status;

        public:
        const StepUp & get_data() const { return data; }
        StepUp & get_mutable_data() { return data; }
        void set_data(const StepUp & value) { this->data = value; }

        const CompletedInstalments & get_status() const { return status; }
        CompletedInstalments & get_mutable_status() { return status; }
        void set_status(const CompletedInstalments & value) { this->status = value; }
    };

    class MfsipInfoClass {
        public:
        MfsipInfoClass() = default;
        virtual ~MfsipInfoClass() = default;

        private:
        bool additional_properties;
        MfsipInfoProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const MfsipInfoProperties & get_properties() const { return properties; }
        MfsipInfoProperties & get_mutable_properties() { return properties; }
        void set_properties(const MfsipInfoProperties & value) { this->properties = value; }

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

    class StepUpProperties {
        public:
        StepUpProperties() = default;
        virtual ~StepUpProperties() = default;

        private:
        CompletedInstalments the_1502;

        public:
        const CompletedInstalments & get_the_1502() const { return the_1502; }
        CompletedInstalments & get_mutable_the_1502() { return the_1502; }
        void set_the_1502(const CompletedInstalments & value) { this->the_1502 = value; }
    };

    class StepUpClass {
        public:
        StepUpClass() = default;
        virtual ~StepUpClass() = default;

        private:
        bool additional_properties;
        StepUpProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const StepUpProperties & get_properties() const { return properties; }
        StepUpProperties & get_mutable_properties() { return properties; }
        void set_properties(const StepUpProperties & value) { this->properties = value; }

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
        MfsipInfoClass mfsip_info;
        StepUpClass step_up;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const MfsipInfoClass & get_mfsip_info() const { return mfsip_info; }
        MfsipInfoClass & get_mutable_mfsip_info() { return mfsip_info; }
        void set_mfsip_info(const MfsipInfoClass & value) { this->mfsip_info = value; }

        const StepUpClass & get_step_up() const { return step_up; }
        StepUpClass & get_mutable_step_up() { return step_up; }
        void set_step_up(const StepUpClass & value) { this->step_up = value; }
    };

    class MfSipInfo {
        public:
        MfSipInfo() = default;
        virtual ~MfSipInfo() = default;

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

    void from_json(const json & j, quicktype::StepUp & x);
    void to_json(json & j, const quicktype::StepUp & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::MfsipInfoProperties & x);
    void to_json(json & j, const quicktype::MfsipInfoProperties & x);

    void from_json(const json & j, quicktype::MfsipInfoClass & x);
    void to_json(json & j, const quicktype::MfsipInfoClass & x);

    void from_json(const json & j, quicktype::StepUpProperties & x);
    void to_json(json & j, const quicktype::StepUpProperties & x);

    void from_json(const json & j, quicktype::StepUpClass & x);
    void to_json(json & j, const quicktype::StepUpClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::MfSipInfo & x);
    void to_json(json & j, const quicktype::MfSipInfo & x);

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
        x.set_format(j.at("format").get<std::string>());
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::Created & x) {
        j = json::object();
        j["format"] = x.get_format();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::StepUp& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::StepUp & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_completed_instalments(j.at("completed_instalments").get<quicktype::CompletedInstalments>());
        x.set_created(j.at("created").get<quicktype::Created>());
        x.set_dividend_type(j.at("dividend_type").get<quicktype::CompletedInstalments>());
        x.set_frequency(j.at("frequency").get<quicktype::CompletedInstalments>());
        x.set_fund(j.at("fund").get<quicktype::CompletedInstalments>());
        x.set_fund_source(j.at("fund_source").get<quicktype::CompletedInstalments>());
        x.set_instalment_amount(j.at("instalment_amount").get<quicktype::CompletedInstalments>());
        x.set_instalment_day(j.at("instalment_day").get<quicktype::CompletedInstalments>());
        x.set_instalments(j.at("instalments").get<quicktype::CompletedInstalments>());
        x.set_last_instalment(j.at("last_instalment").get<quicktype::Created>());
        x.set_next_instalment(j.at("next_instalment").get<quicktype::Created>());
        x.set_pending_instalments(j.at("pending_instalments").get<quicktype::CompletedInstalments>());
        x.set_sip_id(j.at("sip_id").get<quicktype::CompletedInstalments>());
        x.set_sip_reg_num(j.at("sip_reg_num").get<quicktype::CompletedInstalments>());
        x.set_sip_type(j.at("sip_type").get<quicktype::CompletedInstalments>());
        x.set_status(j.at("status").get<quicktype::CompletedInstalments>());
        x.set_step_up(j.at("step_up").get<quicktype::StepUp>());
        x.set_tag(j.at("tag").get<quicktype::CompletedInstalments>());
        x.set_tradingsymbol(j.at("tradingsymbol").get<quicktype::CompletedInstalments>());
        x.set_transaction_type(j.at("transaction_type").get<quicktype::CompletedInstalments>());
        x.set_trigger_price(j.at("trigger_price").get<quicktype::CompletedInstalments>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["completed_instalments"] = x.get_completed_instalments();
        j["created"] = x.get_created();
        j["dividend_type"] = x.get_dividend_type();
        j["frequency"] = x.get_frequency();
        j["fund"] = x.get_fund();
        j["fund_source"] = x.get_fund_source();
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

    inline void from_json(const json & j, quicktype::MfsipInfoProperties& x) {
        x.set_data(j.at("data").get<quicktype::StepUp>());
        x.set_status(j.at("status").get<quicktype::CompletedInstalments>());
    }

    inline void to_json(json & j, const quicktype::MfsipInfoProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::MfsipInfoClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::MfsipInfoProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MfsipInfoClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::StepUpProperties& x) {
        x.set_the_1502(j.at("15-02").get<quicktype::CompletedInstalments>());
    }

    inline void to_json(json & j, const quicktype::StepUpProperties & x) {
        j = json::object();
        j["15-02"] = x.get_the_1502();
    }

    inline void from_json(const json & j, quicktype::StepUpClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::StepUpProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::StepUpClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_mfsip_info(j.at("MFSIPInfo").get<quicktype::MfsipInfoClass>());
        x.set_step_up(j.at("StepUp").get<quicktype::StepUpClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Data"] = x.get_data();
        j["MFSIPInfo"] = x.get_mfsip_info();
        j["StepUp"] = x.get_step_up();
    }

    inline void from_json(const json & j, quicktype::MfSipInfo& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::MfSipInfo & x) {
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
