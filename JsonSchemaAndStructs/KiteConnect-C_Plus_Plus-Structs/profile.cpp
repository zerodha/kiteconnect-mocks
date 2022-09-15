//  To parse this JSON data, first install
//
//      Boost     http://www.boost.org
//      json.hpp  https://github.com/nlohmann/json
//
//  Then include this file, and then do
//
//     Profile data = nlohmann::json::parse(jsonString);

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

    enum class Type : int { STRING, TYPE_NULL };

    class AvatarUrl {
        public:
        AvatarUrl() = default;
        virtual ~AvatarUrl() = default;

        private:
        Type type;

        public:
        const Type & get_type() const { return type; }
        Type & get_mutable_type() { return type; }
        void set_type(const Type & value) { this->type = value; }
    };

    class Exchanges {
        public:
        Exchanges() = default;
        virtual ~Exchanges() = default;

        private:
        AvatarUrl items;
        std::string type;

        public:
        const AvatarUrl & get_items() const { return items; }
        AvatarUrl & get_mutable_items() { return items; }
        void set_items(const AvatarUrl & value) { this->items = value; }

        const std::string & get_type() const { return type; }
        std::string & get_mutable_type() { return type; }
        void set_type(const std::string & value) { this->type = value; }
    };

    class Meta {
        public:
        Meta() = default;
        virtual ~Meta() = default;

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
        AvatarUrl avatar_url;
        AvatarUrl broker;
        AvatarUrl email;
        Exchanges exchanges;
        Meta meta;
        Exchanges order_types;
        Exchanges products;
        AvatarUrl user_id;
        AvatarUrl user_name;
        AvatarUrl user_shortname;
        AvatarUrl user_type;

        public:
        const AvatarUrl & get_avatar_url() const { return avatar_url; }
        AvatarUrl & get_mutable_avatar_url() { return avatar_url; }
        void set_avatar_url(const AvatarUrl & value) { this->avatar_url = value; }

        const AvatarUrl & get_broker() const { return broker; }
        AvatarUrl & get_mutable_broker() { return broker; }
        void set_broker(const AvatarUrl & value) { this->broker = value; }

        const AvatarUrl & get_email() const { return email; }
        AvatarUrl & get_mutable_email() { return email; }
        void set_email(const AvatarUrl & value) { this->email = value; }

        const Exchanges & get_exchanges() const { return exchanges; }
        Exchanges & get_mutable_exchanges() { return exchanges; }
        void set_exchanges(const Exchanges & value) { this->exchanges = value; }

        const Meta & get_meta() const { return meta; }
        Meta & get_mutable_meta() { return meta; }
        void set_meta(const Meta & value) { this->meta = value; }

        const Exchanges & get_order_types() const { return order_types; }
        Exchanges & get_mutable_order_types() { return order_types; }
        void set_order_types(const Exchanges & value) { this->order_types = value; }

        const Exchanges & get_products() const { return products; }
        Exchanges & get_mutable_products() { return products; }
        void set_products(const Exchanges & value) { this->products = value; }

        const AvatarUrl & get_user_id() const { return user_id; }
        AvatarUrl & get_mutable_user_id() { return user_id; }
        void set_user_id(const AvatarUrl & value) { this->user_id = value; }

        const AvatarUrl & get_user_name() const { return user_name; }
        AvatarUrl & get_mutable_user_name() { return user_name; }
        void set_user_name(const AvatarUrl & value) { this->user_name = value; }

        const AvatarUrl & get_user_shortname() const { return user_shortname; }
        AvatarUrl & get_mutable_user_shortname() { return user_shortname; }
        void set_user_shortname(const AvatarUrl & value) { this->user_shortname = value; }

        const AvatarUrl & get_user_type() const { return user_type; }
        AvatarUrl & get_mutable_user_type() { return user_type; }
        void set_user_type(const AvatarUrl & value) { this->user_type = value; }
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

    class MetaProperties {
        public:
        MetaProperties() = default;
        virtual ~MetaProperties() = default;

        private:
        AvatarUrl demat_consent;

        public:
        const AvatarUrl & get_demat_consent() const { return demat_consent; }
        AvatarUrl & get_mutable_demat_consent() { return demat_consent; }
        void set_demat_consent(const AvatarUrl & value) { this->demat_consent = value; }
    };

    class MetaClass {
        public:
        MetaClass() = default;
        virtual ~MetaClass() = default;

        private:
        bool additional_properties;
        MetaProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const MetaProperties & get_properties() const { return properties; }
        MetaProperties & get_mutable_properties() { return properties; }
        void set_properties(const MetaProperties & value) { this->properties = value; }

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

    class ProfileProperties {
        public:
        ProfileProperties() = default;
        virtual ~ProfileProperties() = default;

        private:
        Meta data;
        AvatarUrl status;

        public:
        const Meta & get_data() const { return data; }
        Meta & get_mutable_data() { return data; }
        void set_data(const Meta & value) { this->data = value; }

        const AvatarUrl & get_status() const { return status; }
        AvatarUrl & get_mutable_status() { return status; }
        void set_status(const AvatarUrl & value) { this->status = value; }
    };

    class ProfileClass {
        public:
        ProfileClass() = default;
        virtual ~ProfileClass() = default;

        private:
        bool additional_properties;
        ProfileProperties properties;
        std::vector<std::string> required;
        std::string title;
        std::string type;

        public:
        const bool & get_additional_properties() const { return additional_properties; }
        bool & get_mutable_additional_properties() { return additional_properties; }
        void set_additional_properties(const bool & value) { this->additional_properties = value; }

        const ProfileProperties & get_properties() const { return properties; }
        ProfileProperties & get_mutable_properties() { return properties; }
        void set_properties(const ProfileProperties & value) { this->properties = value; }

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
        MetaClass meta;
        ProfileClass profile;

        public:
        const Data & get_data() const { return data; }
        Data & get_mutable_data() { return data; }
        void set_data(const Data & value) { this->data = value; }

        const MetaClass & get_meta() const { return meta; }
        MetaClass & get_mutable_meta() { return meta; }
        void set_meta(const MetaClass & value) { this->meta = value; }

        const ProfileClass & get_profile() const { return profile; }
        ProfileClass & get_mutable_profile() { return profile; }
        void set_profile(const ProfileClass & value) { this->profile = value; }
    };

    class Profile {
        public:
        Profile() = default;
        virtual ~Profile() = default;

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
    void from_json(const json & j, quicktype::AvatarUrl & x);
    void to_json(json & j, const quicktype::AvatarUrl & x);

    void from_json(const json & j, quicktype::Exchanges & x);
    void to_json(json & j, const quicktype::Exchanges & x);

    void from_json(const json & j, quicktype::Meta & x);
    void to_json(json & j, const quicktype::Meta & x);

    void from_json(const json & j, quicktype::DataProperties & x);
    void to_json(json & j, const quicktype::DataProperties & x);

    void from_json(const json & j, quicktype::Data & x);
    void to_json(json & j, const quicktype::Data & x);

    void from_json(const json & j, quicktype::MetaProperties & x);
    void to_json(json & j, const quicktype::MetaProperties & x);

    void from_json(const json & j, quicktype::MetaClass & x);
    void to_json(json & j, const quicktype::MetaClass & x);

    void from_json(const json & j, quicktype::ProfileProperties & x);
    void to_json(json & j, const quicktype::ProfileProperties & x);

    void from_json(const json & j, quicktype::ProfileClass & x);
    void to_json(json & j, const quicktype::ProfileClass & x);

    void from_json(const json & j, quicktype::Definitions & x);
    void to_json(json & j, const quicktype::Definitions & x);

    void from_json(const json & j, quicktype::Profile & x);
    void to_json(json & j, const quicktype::Profile & x);

    void from_json(const json & j, quicktype::Type & x);
    void to_json(json & j, const quicktype::Type & x);

    inline void from_json(const json & j, quicktype::AvatarUrl& x) {
        x.set_type(j.at("type").get<quicktype::Type>());
    }

    inline void to_json(json & j, const quicktype::AvatarUrl & x) {
        j = json::object();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Exchanges& x) {
        x.set_items(j.at("items").get<quicktype::AvatarUrl>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Exchanges & x) {
        j = json::object();
        j["items"] = x.get_items();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Meta& x) {
        x.set_ref(j.at("$ref").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::Meta & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
    }

    inline void from_json(const json & j, quicktype::DataProperties& x) {
        x.set_avatar_url(j.at("avatar_url").get<quicktype::AvatarUrl>());
        x.set_broker(j.at("broker").get<quicktype::AvatarUrl>());
        x.set_email(j.at("email").get<quicktype::AvatarUrl>());
        x.set_exchanges(j.at("exchanges").get<quicktype::Exchanges>());
        x.set_meta(j.at("meta").get<quicktype::Meta>());
        x.set_order_types(j.at("order_types").get<quicktype::Exchanges>());
        x.set_products(j.at("products").get<quicktype::Exchanges>());
        x.set_user_id(j.at("user_id").get<quicktype::AvatarUrl>());
        x.set_user_name(j.at("user_name").get<quicktype::AvatarUrl>());
        x.set_user_shortname(j.at("user_shortname").get<quicktype::AvatarUrl>());
        x.set_user_type(j.at("user_type").get<quicktype::AvatarUrl>());
    }

    inline void to_json(json & j, const quicktype::DataProperties & x) {
        j = json::object();
        j["avatar_url"] = x.get_avatar_url();
        j["broker"] = x.get_broker();
        j["email"] = x.get_email();
        j["exchanges"] = x.get_exchanges();
        j["meta"] = x.get_meta();
        j["order_types"] = x.get_order_types();
        j["products"] = x.get_products();
        j["user_id"] = x.get_user_id();
        j["user_name"] = x.get_user_name();
        j["user_shortname"] = x.get_user_shortname();
        j["user_type"] = x.get_user_type();
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

    inline void from_json(const json & j, quicktype::MetaProperties& x) {
        x.set_demat_consent(j.at("demat_consent").get<quicktype::AvatarUrl>());
    }

    inline void to_json(json & j, const quicktype::MetaProperties & x) {
        j = json::object();
        j["demat_consent"] = x.get_demat_consent();
    }

    inline void from_json(const json & j, quicktype::MetaClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::MetaProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::MetaClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::ProfileProperties& x) {
        x.set_data(j.at("data").get<quicktype::Meta>());
        x.set_status(j.at("status").get<quicktype::AvatarUrl>());
    }

    inline void to_json(json & j, const quicktype::ProfileProperties & x) {
        j = json::object();
        j["data"] = x.get_data();
        j["status"] = x.get_status();
    }

    inline void from_json(const json & j, quicktype::ProfileClass& x) {
        x.set_additional_properties(j.at("additionalProperties").get<bool>());
        x.set_properties(j.at("properties").get<quicktype::ProfileProperties>());
        x.set_required(j.at("required").get<std::vector<std::string>>());
        x.set_title(j.at("title").get<std::string>());
        x.set_type(j.at("type").get<std::string>());
    }

    inline void to_json(json & j, const quicktype::ProfileClass & x) {
        j = json::object();
        j["additionalProperties"] = x.get_additional_properties();
        j["properties"] = x.get_properties();
        j["required"] = x.get_required();
        j["title"] = x.get_title();
        j["type"] = x.get_type();
    }

    inline void from_json(const json & j, quicktype::Definitions& x) {
        x.set_data(j.at("Data").get<quicktype::Data>());
        x.set_meta(j.at("Meta").get<quicktype::MetaClass>());
        x.set_profile(j.at("Profile").get<quicktype::ProfileClass>());
    }

    inline void to_json(json & j, const quicktype::Definitions & x) {
        j = json::object();
        j["Data"] = x.get_data();
        j["Meta"] = x.get_meta();
        j["Profile"] = x.get_profile();
    }

    inline void from_json(const json & j, quicktype::Profile& x) {
        x.set_ref(j.at("$ref").get<std::string>());
        x.set_schema(j.at("$schema").get<std::string>());
        x.set_definitions(j.at("definitions").get<quicktype::Definitions>());
    }

    inline void to_json(json & j, const quicktype::Profile & x) {
        j = json::object();
        j["$ref"] = x.get_ref();
        j["$schema"] = x.get_schema();
        j["definitions"] = x.get_definitions();
    }

    inline void from_json(const json & j, quicktype::Type & x) {
        if (j == "string") x = quicktype::Type::STRING;
        else if (j == "null") x = quicktype::Type::TYPE_NULL;
        else throw "Input JSON does not conform to schema";
    }

    inline void to_json(json & j, const quicktype::Type & x) {
        switch (x) {
            case quicktype::Type::STRING: j = "string"; break;
            case quicktype::Type::TYPE_NULL: j = "null"; break;
            default: throw "This should not happen";
        }
    }
}
