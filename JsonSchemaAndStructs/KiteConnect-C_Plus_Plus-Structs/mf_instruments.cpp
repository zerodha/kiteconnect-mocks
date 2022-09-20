//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     MfInstruments data = nlohmann::json::parse(jsonString);

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

namespace MfInstruments {
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

    enum class Amc : int { BIRLA_SUN_LIFE_MUTUAL_FUND_MF };

    enum class DividendType : int { GROWTH, PAYOUT };

    enum class Plan : int { DIRECT, REGULAR };

    enum class SchemeType : int { BALANCED, DEBT, EQUITY, FOF, LIQUID };

    enum class SettlementType : int { T1, T3, T4, T6 };

    class MfInstrument {
        public:
        MfInstrument() = default;
        virtual ~MfInstrument() = default;

        private:
        std::shared_ptr<Amc> amc;
        std::shared_ptr<DividendType> dividend_type;
        std::shared_ptr<double> last_price;
        std::shared_ptr<std::string> last_price_date;
        std::shared_ptr<int64_t> minimum_additional_purchase_amount;
        std::shared_ptr<int64_t> minimum_purchase_amount;
        std::shared_ptr<double> minimum_redemption_quantity;
        std::shared_ptr<std::string> name;
        std::shared_ptr<Plan> plan;
        std::shared_ptr<int64_t> purchase_allowed;
        std::shared_ptr<int64_t> purchase_amount_multiplier;
        std::shared_ptr<int64_t> redemption_allowed;
        std::shared_ptr<double> redemption_quantity_multiplier;
        std::shared_ptr<SchemeType> scheme_type;
        std::shared_ptr<SettlementType> settlement_type;
        std::shared_ptr<std::string> tradingsymbol;

        public:
        std::shared_ptr<Amc> get_amc() const { return amc; }
        void set_amc(std::shared_ptr<Amc> value) { this->amc = value; }

        std::shared_ptr<DividendType> get_dividend_type() const { return dividend_type; }
        void set_dividend_type(std::shared_ptr<DividendType> value) { this->dividend_type = value; }

        std::shared_ptr<double> get_last_price() const { return last_price; }
        void set_last_price(std::shared_ptr<double> value) { this->last_price = value; }

        std::shared_ptr<std::string> get_last_price_date() const { return last_price_date; }
        void set_last_price_date(std::shared_ptr<std::string> value) { this->last_price_date = value; }

        std::shared_ptr<int64_t> get_minimum_additional_purchase_amount() const { return minimum_additional_purchase_amount; }
        void set_minimum_additional_purchase_amount(std::shared_ptr<int64_t> value) { this->minimum_additional_purchase_amount = value; }

        std::shared_ptr<int64_t> get_minimum_purchase_amount() const { return minimum_purchase_amount; }
        void set_minimum_purchase_amount(std::shared_ptr<int64_t> value) { this->minimum_purchase_amount = value; }

        std::shared_ptr<double> get_minimum_redemption_quantity() const { return minimum_redemption_quantity; }
        void set_minimum_redemption_quantity(std::shared_ptr<double> value) { this->minimum_redemption_quantity = value; }

        std::shared_ptr<std::string> get_name() const { return name; }
        void set_name(std::shared_ptr<std::string> value) { this->name = value; }

        std::shared_ptr<Plan> get_plan() const { return plan; }
        void set_plan(std::shared_ptr<Plan> value) { this->plan = value; }

        std::shared_ptr<int64_t> get_purchase_allowed() const { return purchase_allowed; }
        void set_purchase_allowed(std::shared_ptr<int64_t> value) { this->purchase_allowed = value; }

        std::shared_ptr<int64_t> get_purchase_amount_multiplier() const { return purchase_amount_multiplier; }
        void set_purchase_amount_multiplier(std::shared_ptr<int64_t> value) { this->purchase_amount_multiplier = value; }

        std::shared_ptr<int64_t> get_redemption_allowed() const { return redemption_allowed; }
        void set_redemption_allowed(std::shared_ptr<int64_t> value) { this->redemption_allowed = value; }

        std::shared_ptr<double> get_redemption_quantity_multiplier() const { return redemption_quantity_multiplier; }
        void set_redemption_quantity_multiplier(std::shared_ptr<double> value) { this->redemption_quantity_multiplier = value; }

        std::shared_ptr<SchemeType> get_scheme_type() const { return scheme_type; }
        void set_scheme_type(std::shared_ptr<SchemeType> value) { this->scheme_type = value; }

        std::shared_ptr<SettlementType> get_settlement_type() const { return settlement_type; }
        void set_settlement_type(std::shared_ptr<SettlementType> value) { this->settlement_type = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }
    };

    using MfInstruments = std::vector<MfInstrument>;
}

namespace MfInstruments {
    using MfInstruments = std::vector<MfInstrument>;
}

namespace nlohmann {
    void from_json(const json & j, MfInstruments::MfInstrument & x);
    void to_json(json & j, const MfInstruments::MfInstrument & x);

    void from_json(const json & j, MfInstruments::Amc & x);
    void to_json(json & j, const MfInstruments::Amc & x);

    void from_json(const json & j, MfInstruments::DividendType & x);
    void to_json(json & j, const MfInstruments::DividendType & x);

    void from_json(const json & j, MfInstruments::Plan & x);
    void to_json(json & j, const MfInstruments::Plan & x);

    void from_json(const json & j, MfInstruments::SchemeType & x);
    void to_json(json & j, const MfInstruments::SchemeType & x);

    void from_json(const json & j, MfInstruments::SettlementType & x);
    void to_json(json & j, const MfInstruments::SettlementType & x);

    inline void from_json(const json & j, MfInstruments::MfInstrument& x) {
        x.set_amc(MfInstruments::get_optional<MfInstruments::Amc>(j, "amc"));
        x.set_dividend_type(MfInstruments::get_optional<MfInstruments::DividendType>(j, "dividend_type"));
        x.set_last_price(MfInstruments::get_optional<double>(j, "last_price"));
        x.set_last_price_date(MfInstruments::get_optional<std::string>(j, "last_price_date"));
        x.set_minimum_additional_purchase_amount(MfInstruments::get_optional<int64_t>(j, "minimum_additional_purchase_amount"));
        x.set_minimum_purchase_amount(MfInstruments::get_optional<int64_t>(j, "minimum_purchase_amount"));
        x.set_minimum_redemption_quantity(MfInstruments::get_optional<double>(j, "minimum_redemption_quantity"));
        x.set_name(MfInstruments::get_optional<std::string>(j, "name"));
        x.set_plan(MfInstruments::get_optional<MfInstruments::Plan>(j, "plan"));
        x.set_purchase_allowed(MfInstruments::get_optional<int64_t>(j, "purchase_allowed"));
        x.set_purchase_amount_multiplier(MfInstruments::get_optional<int64_t>(j, "purchase_amount_multiplier"));
        x.set_redemption_allowed(MfInstruments::get_optional<int64_t>(j, "redemption_allowed"));
        x.set_redemption_quantity_multiplier(MfInstruments::get_optional<double>(j, "redemption_quantity_multiplier"));
        x.set_scheme_type(MfInstruments::get_optional<MfInstruments::SchemeType>(j, "scheme_type"));
        x.set_settlement_type(MfInstruments::get_optional<MfInstruments::SettlementType>(j, "settlement_type"));
        x.set_tradingsymbol(MfInstruments::get_optional<std::string>(j, "tradingsymbol"));
    }

    inline void to_json(json & j, const MfInstruments::MfInstrument & x) {
        j = json::object();
        j["amc"] = x.get_amc();
        j["dividend_type"] = x.get_dividend_type();
        j["last_price"] = x.get_last_price();
        j["last_price_date"] = x.get_last_price_date();
        j["minimum_additional_purchase_amount"] = x.get_minimum_additional_purchase_amount();
        j["minimum_purchase_amount"] = x.get_minimum_purchase_amount();
        j["minimum_redemption_quantity"] = x.get_minimum_redemption_quantity();
        j["name"] = x.get_name();
        j["plan"] = x.get_plan();
        j["purchase_allowed"] = x.get_purchase_allowed();
        j["purchase_amount_multiplier"] = x.get_purchase_amount_multiplier();
        j["redemption_allowed"] = x.get_redemption_allowed();
        j["redemption_quantity_multiplier"] = x.get_redemption_quantity_multiplier();
        j["scheme_type"] = x.get_scheme_type();
        j["settlement_type"] = x.get_settlement_type();
        j["tradingsymbol"] = x.get_tradingsymbol();
    }

    inline void from_json(const json & j, MfInstruments::Amc & x) {
        if (j == "BirlaSunLifeMutualFund_MF") x = MfInstruments::Amc::BIRLA_SUN_LIFE_MUTUAL_FUND_MF;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const MfInstruments::Amc & x) {
        switch (x) {
            case MfInstruments::Amc::BIRLA_SUN_LIFE_MUTUAL_FUND_MF: j = "BirlaSunLifeMutualFund_MF"; break;
            default: throw "This should not happen";
        }
    }

    inline void from_json(const json & j, MfInstruments::DividendType & x) {
        if (j == "growth") x = MfInstruments::DividendType::GROWTH;
        else if (j == "payout") x = MfInstruments::DividendType::PAYOUT;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const MfInstruments::DividendType & x) {
        switch (x) {
            case MfInstruments::DividendType::GROWTH: j = "growth"; break;
            case MfInstruments::DividendType::PAYOUT: j = "payout"; break;
            default: throw "This should not happen";
        }
    }

    inline void from_json(const json & j, MfInstruments::Plan & x) {
        if (j == "direct") x = MfInstruments::Plan::DIRECT;
        else if (j == "regular") x = MfInstruments::Plan::REGULAR;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const MfInstruments::Plan & x) {
        switch (x) {
            case MfInstruments::Plan::DIRECT: j = "direct"; break;
            case MfInstruments::Plan::REGULAR: j = "regular"; break;
            default: throw "This should not happen";
        }
    }

    inline void from_json(const json & j, MfInstruments::SchemeType & x) {
        if (j == "balanced") x = MfInstruments::SchemeType::BALANCED;
        else if (j == "debt") x = MfInstruments::SchemeType::DEBT;
        else if (j == "equity") x = MfInstruments::SchemeType::EQUITY;
        else if (j == "fof") x = MfInstruments::SchemeType::FOF;
        else if (j == "liquid") x = MfInstruments::SchemeType::LIQUID;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const MfInstruments::SchemeType & x) {
        switch (x) {
            case MfInstruments::SchemeType::BALANCED: j = "balanced"; break;
            case MfInstruments::SchemeType::DEBT: j = "debt"; break;
            case MfInstruments::SchemeType::EQUITY: j = "equity"; break;
            case MfInstruments::SchemeType::FOF: j = "fof"; break;
            case MfInstruments::SchemeType::LIQUID: j = "liquid"; break;
            default: throw "This should not happen";
        }
    }

    inline void from_json(const json & j, MfInstruments::SettlementType & x) {
        if (j == "T1") x = MfInstruments::SettlementType::T1;
        else if (j == "T3") x = MfInstruments::SettlementType::T3;
        else if (j == "T4") x = MfInstruments::SettlementType::T4;
        else if (j == "T6") x = MfInstruments::SettlementType::T6;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const MfInstruments::SettlementType & x) {
        switch (x) {
            case MfInstruments::SettlementType::T1: j = "T1"; break;
            case MfInstruments::SettlementType::T3: j = "T3"; break;
            case MfInstruments::SettlementType::T4: j = "T4"; break;
            case MfInstruments::SettlementType::T6: j = "T6"; break;
            default: throw "This should not happen";
        }
    }
}
