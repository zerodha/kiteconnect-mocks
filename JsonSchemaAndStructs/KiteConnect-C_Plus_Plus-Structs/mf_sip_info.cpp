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

namespace MfSipInfo {
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

    class StepUp {
        public:
        StepUp() = default;
        virtual ~StepUp() = default;

        private:
        std::shared_ptr<int64_t> the_1502;

        public:
        std::shared_ptr<int64_t> get_the_1502() const { return the_1502; }
        void set_the_1502(std::shared_ptr<int64_t> value) { this->the_1502 = value; }
    };

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        std::shared_ptr<int64_t> completed_instalments;
        std::shared_ptr<std::string> created;
        std::shared_ptr<std::string> dividend_type;
        std::shared_ptr<std::string> frequency;
        std::shared_ptr<std::string> fund;
        std::shared_ptr<std::string> fund_source;
        std::shared_ptr<int64_t> instalment_amount;
        std::shared_ptr<int64_t> instalment_day;
        std::shared_ptr<int64_t> instalments;
        std::shared_ptr<std::string> last_instalment;
        std::shared_ptr<std::string> next_instalment;
        std::shared_ptr<int64_t> pending_instalments;
        std::shared_ptr<std::string> sip_id;
        nlohmann::json sip_reg_num;
        std::shared_ptr<std::string> sip_type;
        std::shared_ptr<std::string> status;
        std::shared_ptr<StepUp> step_up;
        std::shared_ptr<std::string> tag;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> transaction_type;
        std::shared_ptr<int64_t> trigger_price;

        public:
        std::shared_ptr<int64_t> get_completed_instalments() const { return completed_instalments; }
        void set_completed_instalments(std::shared_ptr<int64_t> value) { this->completed_instalments = value; }

        std::shared_ptr<std::string> get_created() const { return created; }
        void set_created(std::shared_ptr<std::string> value) { this->created = value; }

        std::shared_ptr<std::string> get_dividend_type() const { return dividend_type; }
        void set_dividend_type(std::shared_ptr<std::string> value) { this->dividend_type = value; }

        std::shared_ptr<std::string> get_frequency() const { return frequency; }
        void set_frequency(std::shared_ptr<std::string> value) { this->frequency = value; }

        std::shared_ptr<std::string> get_fund() const { return fund; }
        void set_fund(std::shared_ptr<std::string> value) { this->fund = value; }

        std::shared_ptr<std::string> get_fund_source() const { return fund_source; }
        void set_fund_source(std::shared_ptr<std::string> value) { this->fund_source = value; }

        std::shared_ptr<int64_t> get_instalment_amount() const { return instalment_amount; }
        void set_instalment_amount(std::shared_ptr<int64_t> value) { this->instalment_amount = value; }

        std::shared_ptr<int64_t> get_instalment_day() const { return instalment_day; }
        void set_instalment_day(std::shared_ptr<int64_t> value) { this->instalment_day = value; }

        std::shared_ptr<int64_t> get_instalments() const { return instalments; }
        void set_instalments(std::shared_ptr<int64_t> value) { this->instalments = value; }

        std::shared_ptr<std::string> get_last_instalment() const { return last_instalment; }
        void set_last_instalment(std::shared_ptr<std::string> value) { this->last_instalment = value; }

        std::shared_ptr<std::string> get_next_instalment() const { return next_instalment; }
        void set_next_instalment(std::shared_ptr<std::string> value) { this->next_instalment = value; }

        std::shared_ptr<int64_t> get_pending_instalments() const { return pending_instalments; }
        void set_pending_instalments(std::shared_ptr<int64_t> value) { this->pending_instalments = value; }

        std::shared_ptr<std::string> get_sip_id() const { return sip_id; }
        void set_sip_id(std::shared_ptr<std::string> value) { this->sip_id = value; }

        const nlohmann::json & get_sip_reg_num() const { return sip_reg_num; }
        nlohmann::json & get_mutable_sip_reg_num() { return sip_reg_num; }
        void set_sip_reg_num(const nlohmann::json & value) { this->sip_reg_num = value; }

        std::shared_ptr<std::string> get_sip_type() const { return sip_type; }
        void set_sip_type(std::shared_ptr<std::string> value) { this->sip_type = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }

        std::shared_ptr<StepUp> get_step_up() const { return step_up; }
        void set_step_up(std::shared_ptr<StepUp> value) { this->step_up = value; }

        std::shared_ptr<std::string> get_tag() const { return tag; }
        void set_tag(std::shared_ptr<std::string> value) { this->tag = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_transaction_type() const { return transaction_type; }
        void set_transaction_type(std::shared_ptr<std::string> value) { this->transaction_type = value; }

        std::shared_ptr<int64_t> get_trigger_price() const { return trigger_price; }
        void set_trigger_price(std::shared_ptr<int64_t> value) { this->trigger_price = value; }
    };

    class MfSipInfo {
        public:
        MfSipInfo() = default;
        virtual ~MfSipInfo() = default;

        private:
        std::shared_ptr<Data> data;
        std::shared_ptr<std::string> status;

        public:
        std::shared_ptr<Data> get_data() const { return data; }
        void set_data(std::shared_ptr<Data> value) { this->data = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }
    };
}

namespace nlohmann {
    void from_json(const json & j, MfSipInfo::StepUp & x);
    void to_json(json & j, const MfSipInfo::StepUp & x);

    void from_json(const json & j, MfSipInfo::Data & x);
    void to_json(json & j, const MfSipInfo::Data & x);

    void from_json(const json & j, MfSipInfo::MfSipInfo & x);
    void to_json(json & j, const MfSipInfo::MfSipInfo & x);

    inline void from_json(const json & j, MfSipInfo::StepUp& x) {
        x.set_the_1502(MfSipInfo::get_optional<int64_t>(j, "15-02"));
    }

    inline void to_json(json & j, const MfSipInfo::StepUp & x) {
        j = json::object();
        j["15-02"] = x.get_the_1502();
    }

    inline void from_json(const json & j, MfSipInfo::Data& x) {
        x.set_completed_instalments(MfSipInfo::get_optional<int64_t>(j, "completed_instalments"));
        x.set_created(MfSipInfo::get_optional<std::string>(j, "created"));
        x.set_dividend_type(MfSipInfo::get_optional<std::string>(j, "dividend_type"));
        x.set_frequency(MfSipInfo::get_optional<std::string>(j, "frequency"));
        x.set_fund(MfSipInfo::get_optional<std::string>(j, "fund"));
        x.set_fund_source(MfSipInfo::get_optional<std::string>(j, "fund_source"));
        x.set_instalment_amount(MfSipInfo::get_optional<int64_t>(j, "instalment_amount"));
        x.set_instalment_day(MfSipInfo::get_optional<int64_t>(j, "instalment_day"));
        x.set_instalments(MfSipInfo::get_optional<int64_t>(j, "instalments"));
        x.set_last_instalment(MfSipInfo::get_optional<std::string>(j, "last_instalment"));
        x.set_next_instalment(MfSipInfo::get_optional<std::string>(j, "next_instalment"));
        x.set_pending_instalments(MfSipInfo::get_optional<int64_t>(j, "pending_instalments"));
        x.set_sip_id(MfSipInfo::get_optional<std::string>(j, "sip_id"));
        x.set_sip_reg_num(MfSipInfo::get_untyped(j, "sip_reg_num"));
        x.set_sip_type(MfSipInfo::get_optional<std::string>(j, "sip_type"));
        x.set_status(MfSipInfo::get_optional<std::string>(j, "status"));
        x.set_step_up(MfSipInfo::get_optional<MfSipInfo::StepUp>(j, "step_up"));
        x.set_tag(MfSipInfo::get_optional<std::string>(j, "tag"));
        x.set_tradingsymbol(MfSipInfo::get_optional<std::string>(j, "tradingsymbol"));
        x.set_transaction_type(MfSipInfo::get_optional<std::string>(j, "transaction_type"));
        x.set_trigger_price(MfSipInfo::get_optional<int64_t>(j, "trigger_price"));
    }

    inline void to_json(json & j, const MfSipInfo::Data & x) {
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

    inline void from_json(const json & j, MfSipInfo::MfSipInfo& x) {
        x.set_data(MfSipInfo::get_optional<MfSipInfo::Data>(j, "data"));
        x.set_status(MfSipInfo::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const MfSipInfo::MfSipInfo & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
