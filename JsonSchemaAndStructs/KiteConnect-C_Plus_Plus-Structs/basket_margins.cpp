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

namespace BasketMargins {
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

    class Pnl {
        public:
        Pnl() = default;
        virtual ~Pnl() = default;

        private:
        std::shared_ptr<int64_t> realised;
        std::shared_ptr<int64_t> unrealised;

        public:
        std::shared_ptr<int64_t> get_realised() const { return realised; }
        void set_realised(std::shared_ptr<int64_t> value) { this->realised = value; }

        std::shared_ptr<int64_t> get_unrealised() const { return unrealised; }
        void set_unrealised(std::shared_ptr<int64_t> value) { this->unrealised = value; }
    };

    class Final {
        public:
        Final() = default;
        virtual ~Final() = default;

        private:
        std::shared_ptr<int64_t> additional;
        std::shared_ptr<int64_t> bo;
        std::shared_ptr<int64_t> cash;
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<double> exposure;
        std::shared_ptr<double> option_premium;
        std::shared_ptr<Pnl> pnl;
        std::shared_ptr<double> span;
        std::shared_ptr<double> total;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> type;
        std::shared_ptr<int64_t> var;

        public:
        std::shared_ptr<int64_t> get_additional() const { return additional; }
        void set_additional(std::shared_ptr<int64_t> value) { this->additional = value; }

        std::shared_ptr<int64_t> get_bo() const { return bo; }
        void set_bo(std::shared_ptr<int64_t> value) { this->bo = value; }

        std::shared_ptr<int64_t> get_cash() const { return cash; }
        void set_cash(std::shared_ptr<int64_t> value) { this->cash = value; }

        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<double> get_exposure() const { return exposure; }
        void set_exposure(std::shared_ptr<double> value) { this->exposure = value; }

        std::shared_ptr<double> get_option_premium() const { return option_premium; }
        void set_option_premium(std::shared_ptr<double> value) { this->option_premium = value; }

        std::shared_ptr<Pnl> get_pnl() const { return pnl; }
        void set_pnl(std::shared_ptr<Pnl> value) { this->pnl = value; }

        std::shared_ptr<double> get_span() const { return span; }
        void set_span(std::shared_ptr<double> value) { this->span = value; }

        std::shared_ptr<double> get_total() const { return total; }
        void set_total(std::shared_ptr<double> value) { this->total = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_type() const { return type; }
        void set_type(std::shared_ptr<std::string> value) { this->type = value; }

        std::shared_ptr<int64_t> get_var() const { return var; }
        void set_var(std::shared_ptr<int64_t> value) { this->var = value; }
    };

    class Data {
        public:
        Data() = default;
        virtual ~Data() = default;

        private:
        std::shared_ptr<Final> data_final;
        std::shared_ptr<Final> initial;
        std::shared_ptr<std::vector<Final>> orders;

        public:
        std::shared_ptr<Final> get_data_final() const { return data_final; }
        void set_data_final(std::shared_ptr<Final> value) { this->data_final = value; }

        std::shared_ptr<Final> get_initial() const { return initial; }
        void set_initial(std::shared_ptr<Final> value) { this->initial = value; }

        std::shared_ptr<std::vector<Final>> get_orders() const { return orders; }
        void set_orders(std::shared_ptr<std::vector<Final>> value) { this->orders = value; }
    };

    class BasketMargins {
        public:
        BasketMargins() = default;
        virtual ~BasketMargins() = default;

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
    void from_json(const json & j, BasketMargins::Pnl & x);
    void to_json(json & j, const BasketMargins::Pnl & x);

    void from_json(const json & j, BasketMargins::Final & x);
    void to_json(json & j, const BasketMargins::Final & x);

    void from_json(const json & j, BasketMargins::Data & x);
    void to_json(json & j, const BasketMargins::Data & x);

    void from_json(const json & j, BasketMargins::BasketMargins & x);
    void to_json(json & j, const BasketMargins::BasketMargins & x);

    inline void from_json(const json & j, BasketMargins::Pnl& x) {
        x.set_realised(BasketMargins::get_optional<int64_t>(j, "realised"));
        x.set_unrealised(BasketMargins::get_optional<int64_t>(j, "unrealised"));
    }

    inline void to_json(json & j, const BasketMargins::Pnl & x) {
        j = json::object();
        j["realised"] = x.get_realised();
        j["unrealised"] = x.get_unrealised();
    }

    inline void from_json(const json & j, BasketMargins::Final& x) {
        x.set_additional(BasketMargins::get_optional<int64_t>(j, "additional"));
        x.set_bo(BasketMargins::get_optional<int64_t>(j, "bo"));
        x.set_cash(BasketMargins::get_optional<int64_t>(j, "cash"));
        x.set_exchange(BasketMargins::get_optional<std::string>(j, "exchange"));
        x.set_exposure(BasketMargins::get_optional<double>(j, "exposure"));
        x.set_option_premium(BasketMargins::get_optional<double>(j, "option_premium"));
        x.set_pnl(BasketMargins::get_optional<BasketMargins::Pnl>(j, "pnl"));
        x.set_span(BasketMargins::get_optional<double>(j, "span"));
        x.set_total(BasketMargins::get_optional<double>(j, "total"));
        x.set_tradingsymbol(BasketMargins::get_optional<std::string>(j, "tradingsymbol"));
        x.set_type(BasketMargins::get_optional<std::string>(j, "type"));
        x.set_var(BasketMargins::get_optional<int64_t>(j, "var"));
    }

    inline void to_json(json & j, const BasketMargins::Final & x) {
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

    inline void from_json(const json & j, BasketMargins::Data& x) {
        x.set_data_final(BasketMargins::get_optional<BasketMargins::Final>(j, "final"));
        x.set_initial(BasketMargins::get_optional<BasketMargins::Final>(j, "initial"));
        x.set_orders(BasketMargins::get_optional<std::vector<BasketMargins::Final>>(j, "orders"));
    }

    inline void to_json(json & j, const BasketMargins::Data & x) {
        j = json::object();
        j["final"] = x.get_data_final();
        j["initial"] = x.get_initial();
        j["orders"] = x.get_orders();
    }

    inline void from_json(const json & j, BasketMargins::BasketMargins& x) {
        x.set_data(BasketMargins::get_optional<BasketMargins::Data>(j, "data"));
        x.set_status(BasketMargins::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const BasketMargins::BasketMargins & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
