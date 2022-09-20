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

namespace OrderMargins {
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

    class Datum {
        public:
        Datum() = default;
        virtual ~Datum() = default;

        private:
        std::shared_ptr<int64_t> additional;
        std::shared_ptr<int64_t> bo;
        std::shared_ptr<int64_t> cash;
        std::shared_ptr<std::string> exchange;
        std::shared_ptr<int64_t> exposure;
        std::shared_ptr<int64_t> option_premium;
        std::shared_ptr<Pnl> pnl;
        std::shared_ptr<int64_t> span;
        std::shared_ptr<double> total;
        std::shared_ptr<std::string> tradingsymbol;
        std::shared_ptr<std::string> type;
        std::shared_ptr<double> var;

        public:
        std::shared_ptr<int64_t> get_additional() const { return additional; }
        void set_additional(std::shared_ptr<int64_t> value) { this->additional = value; }

        std::shared_ptr<int64_t> get_bo() const { return bo; }
        void set_bo(std::shared_ptr<int64_t> value) { this->bo = value; }

        std::shared_ptr<int64_t> get_cash() const { return cash; }
        void set_cash(std::shared_ptr<int64_t> value) { this->cash = value; }

        std::shared_ptr<std::string> get_exchange() const { return exchange; }
        void set_exchange(std::shared_ptr<std::string> value) { this->exchange = value; }

        std::shared_ptr<int64_t> get_exposure() const { return exposure; }
        void set_exposure(std::shared_ptr<int64_t> value) { this->exposure = value; }

        std::shared_ptr<int64_t> get_option_premium() const { return option_premium; }
        void set_option_premium(std::shared_ptr<int64_t> value) { this->option_premium = value; }

        std::shared_ptr<Pnl> get_pnl() const { return pnl; }
        void set_pnl(std::shared_ptr<Pnl> value) { this->pnl = value; }

        std::shared_ptr<int64_t> get_span() const { return span; }
        void set_span(std::shared_ptr<int64_t> value) { this->span = value; }

        std::shared_ptr<double> get_total() const { return total; }
        void set_total(std::shared_ptr<double> value) { this->total = value; }

        std::shared_ptr<std::string> get_tradingsymbol() const { return tradingsymbol; }
        void set_tradingsymbol(std::shared_ptr<std::string> value) { this->tradingsymbol = value; }

        std::shared_ptr<std::string> get_type() const { return type; }
        void set_type(std::shared_ptr<std::string> value) { this->type = value; }

        std::shared_ptr<double> get_var() const { return var; }
        void set_var(std::shared_ptr<double> value) { this->var = value; }
    };

    class OrderMargins {
        public:
        OrderMargins() = default;
        virtual ~OrderMargins() = default;

        private:
        std::shared_ptr<std::vector<Datum>> data;
        std::shared_ptr<std::string> status;

        public:
        std::shared_ptr<std::vector<Datum>> get_data() const { return data; }
        void set_data(std::shared_ptr<std::vector<Datum>> value) { this->data = value; }

        std::shared_ptr<std::string> get_status() const { return status; }
        void set_status(std::shared_ptr<std::string> value) { this->status = value; }
    };
}

namespace nlohmann {
    void from_json(const json & j, OrderMargins::Pnl & x);
    void to_json(json & j, const OrderMargins::Pnl & x);

    void from_json(const json & j, OrderMargins::Datum & x);
    void to_json(json & j, const OrderMargins::Datum & x);

    void from_json(const json & j, OrderMargins::OrderMargins & x);
    void to_json(json & j, const OrderMargins::OrderMargins & x);

    inline void from_json(const json & j, OrderMargins::Pnl& x) {
        x.set_realised(OrderMargins::get_optional<int64_t>(j, "realised"));
        x.set_unrealised(OrderMargins::get_optional<int64_t>(j, "unrealised"));
    }

    inline void to_json(json & j, const OrderMargins::Pnl & x) {
        j = json::object();
        j["realised"] = x.get_realised();
        j["unrealised"] = x.get_unrealised();
    }

    inline void from_json(const json & j, OrderMargins::Datum& x) {
        x.set_additional(OrderMargins::get_optional<int64_t>(j, "additional"));
        x.set_bo(OrderMargins::get_optional<int64_t>(j, "bo"));
        x.set_cash(OrderMargins::get_optional<int64_t>(j, "cash"));
        x.set_exchange(OrderMargins::get_optional<std::string>(j, "exchange"));
        x.set_exposure(OrderMargins::get_optional<int64_t>(j, "exposure"));
        x.set_option_premium(OrderMargins::get_optional<int64_t>(j, "option_premium"));
        x.set_pnl(OrderMargins::get_optional<OrderMargins::Pnl>(j, "pnl"));
        x.set_span(OrderMargins::get_optional<int64_t>(j, "span"));
        x.set_total(OrderMargins::get_optional<double>(j, "total"));
        x.set_tradingsymbol(OrderMargins::get_optional<std::string>(j, "tradingsymbol"));
        x.set_type(OrderMargins::get_optional<std::string>(j, "type"));
        x.set_var(OrderMargins::get_optional<double>(j, "var"));
    }

    inline void to_json(json & j, const OrderMargins::Datum & x) {
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

    inline void from_json(const json & j, OrderMargins::OrderMargins& x) {
        x.set_data(OrderMargins::get_optional<std::vector<OrderMargins::Datum>>(j, "data"));
        x.set_status(OrderMargins::get_optional<std::string>(j, "status"));
    }

    inline void to_json(json & j, const OrderMargins::OrderMargins & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }
}
