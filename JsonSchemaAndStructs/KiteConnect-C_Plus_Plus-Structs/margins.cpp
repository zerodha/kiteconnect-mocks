//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Margins data = nlohmann::json::parse(jsonString);

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

namespace Margins {
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

    class Available {
        public:
        Available() = default;
        virtual ~Available() = default;

        private:
        std::shared_ptr<int64_t> adhoc_margin;
        std::shared_ptr<double> cash;
        std::shared_ptr<int64_t> collateral;
        std::shared_ptr<int64_t> intraday_payin;
        std::shared_ptr<double> live_balance;
        std::shared_ptr<double> opening_balance;

        public:
        std::shared_ptr<int64_t> get_adhoc_margin() const { return adhoc_margin; }
        void set_adhoc_margin(std::shared_ptr<int64_t> value) { this->adhoc_margin = value; }

        std::shared_ptr<double> get_cash() const { return cash; }
        void set_cash(std::shared_ptr<double> value) { this->cash = value; }

        std::shared_ptr<int64_t> get_collateral() const { return collateral; }
        void set_collateral(std::shared_ptr<int64_t> value) { this->collateral = value; }

        std::shared_ptr<int64_t> get_intraday_payin() const { return intraday_payin; }
        void set_intraday_payin(std::shared_ptr<int64_t> value) { this->intraday_payin = value; }

        std::shared_ptr<double> get_live_balance() const { return live_balance; }
        void set_live_balance(std::shared_ptr<double> value) { this->live_balance = value; }

        std::shared_ptr<double> get_opening_balance() const { return opening_balance; }
        void set_opening_balance(std::shared_ptr<double> value) { this->opening_balance = value; }
    };

    class Ity {
        public:
        Ity() = default;
        virtual ~Ity() = default;

        private:
        std::shared_ptr<Available> available;
        std::shared_ptr<bool> enabled;
        std::shared_ptr<double> net;
        std::shared_ptr<std::map<std::string, double>> utilised;

        public:
        std::shared_ptr<Available> get_available() const { return available; }
        void set_available(std::shared_ptr<Available> value) { this->available = value; }

        std::shared_ptr<bool> get_enabled() const { return enabled; }
        void set_enabled(std::shared_ptr<bool> value) { this->enabled = value; }

        std::shared_ptr<double> get_net() const { return net; }
        void set_net(std::shared_ptr<double> value) { this->net = value; }

        std::shared_ptr<std::map<std::string, double>> get_utilised() const { return utilised; }
        void set_utilised(std::shared_ptr<std::map<std::string, double>> value) { this->utilised = value; }
    };

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        std::shared_ptr<Ity> commodity;
        std::shared_ptr<Ity> equity;

        public:
        std::shared_ptr<Ity> get_commodity() const { return commodity; }
        void set_commodity(std::shared_ptr<Ity> value) { this->commodity = value; }

        std::shared_ptr<Ity> get_equity() const { return equity; }
        void set_equity(std::shared_ptr<Ity> value) { this->equity = value; }
    };

    class Margins {
        public:
        Margins() = default;
        virtual ~Margins() = default;

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
    void from_json(const json & j, Margins::Available & x);
    void to_json(json & j, const Margins::Available & x);

    void from_json(const json & j, Margins::Ity & x);
    void to_json(json & j, const Margins::Ity & x);

    void from_json(const json & j, Margins::Data & x);
    void to_json(json & j, const Margins::Data & x);

    void from_json(const json & j, Margins::Margins & x);
    void to_json(json & j, const Margins::Margins & x);

    inline void from_json(const json & j, Margins::Available& x) {
        x.set_adhoc_margin(Margins::get_optional<int64_t>(j, "adhoc_margin"));
        x.set_cash(Margins::get_optional<double>(j, "cash"));
        x.set_collateral(Margins::get_optional<int64_t>(j, "collateral"));
        x.set_intraday_payin(Margins::get_optional<int64_t>(j, "intraday_payin"));
        x.set_live_balance(Margins::get_optional<double>(j, "live_balance"));
        x.set_opening_balance(Margins::get_optional<double>(j, "opening_balance"));
    }

    inline void to_json(json & j, const Margins::Available & x) {
        j = json::object();
        j["adhoc_margin"] = x.get_adhoc_margin();
        j["cash"] = x.get_cash();
        j["collateral"] = x.get_collateral();
        j["intraday_payin"] = x.get_intraday_payin();
        j["live_balance"] = x.get_live_balance();
        j["opening_balance"] = x.get_opening_balance();
    }

    inline void from_json(const json & j, Margins::Ity& x) {
        x.set_available(Margins::get_optional<Margins::Available>(j, "available"));
        x.set_enabled(Margins::get_optional<bool>(j, "enabled"));
        x.set_net(Margins::get_optional<double>(j, "net"));
        x.set_utilised(Margins::get_optional<std::map<std::string, double>>(j, "utilised"));
    }

    inline void to_json(json & j, const Margins::Ity & x) {
        j = json::object();
        j["available"] = x.get_available();
        j["enabled"] = x.get_enabled();
        j["net"] = x.get_net();
        j["utilised"] = x.get_utilised();
    }

    inline void from_json(const json & j, Margins::Data& x) {
        x.set_commodity(Margins::get_optional<Margins::Ity>(j, "commodity"));
        x.set_equity(Margins::get_optional<Margins::Ity>(j, "equity"));
    }

    inline void to_json(json & j, const Margins::Data & x) {
        j = json::object();
        j["commodity"] = x.get_commodity();
        j["equity"] = x.get_equity();
    }

    inline void from_json(const json & j, Margins::Margins& x) {
        x.set_data(Margins::get_optional<Margins::Data>(j, "data"));
        x.set_status(Margins::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const Margins::Margins & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
