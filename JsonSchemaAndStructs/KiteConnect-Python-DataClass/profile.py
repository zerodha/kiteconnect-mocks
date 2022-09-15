# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = profile_from_dict(json.loads(json_string))

from enum import Enum
from typing import Any, List, TypeVar, Type, cast, Callable


T = TypeVar("T")
EnumT = TypeVar("EnumT", bound=Enum)


def to_enum(c: Type[EnumT], x: Any) -> EnumT:
    assert isinstance(x, c)
    return x.value


def from_str(x: Any) -> str:
    assert isinstance(x, str)
    return x


def to_class(c: Type[T], x: Any) -> dict:
    assert isinstance(x, c)
    return cast(Any, x).to_dict()


def from_bool(x: Any) -> bool:
    assert isinstance(x, bool)
    return x


def from_list(f: Callable[[Any], T], x: Any) -> List[T]:
    assert isinstance(x, list)
    return [f(y) for y in x]


class TypeEnum(Enum):
    NULL = "null"
    STRING = "string"


class AvatarURL:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'AvatarURL':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return AvatarURL(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class Exchanges:
    items: AvatarURL
    type: str

    def __init__(self, items: AvatarURL, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Exchanges':
        assert isinstance(obj, dict)
        items = AvatarURL.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Exchanges(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(AvatarURL, self.items)
        result["type"] = from_str(self.type)
        return result


class Meta:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'Meta':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return Meta(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class DataProperties:
    avatar_url: AvatarURL
    broker: AvatarURL
    email: AvatarURL
    exchanges: Exchanges
    meta: Meta
    order_types: Exchanges
    products: Exchanges
    user_id: AvatarURL
    user_name: AvatarURL
    user_shortname: AvatarURL
    user_type: AvatarURL

    def __init__(self, avatar_url: AvatarURL, broker: AvatarURL, email: AvatarURL, exchanges: Exchanges, meta: Meta, order_types: Exchanges, products: Exchanges, user_id: AvatarURL, user_name: AvatarURL, user_shortname: AvatarURL, user_type: AvatarURL) -> None:
        self.avatar_url = avatar_url
        self.broker = broker
        self.email = email
        self.exchanges = exchanges
        self.meta = meta
        self.order_types = order_types
        self.products = products
        self.user_id = user_id
        self.user_name = user_name
        self.user_shortname = user_shortname
        self.user_type = user_type

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        avatar_url = AvatarURL.from_dict(obj.get("avatar_url"))
        broker = AvatarURL.from_dict(obj.get("broker"))
        email = AvatarURL.from_dict(obj.get("email"))
        exchanges = Exchanges.from_dict(obj.get("exchanges"))
        meta = Meta.from_dict(obj.get("meta"))
        order_types = Exchanges.from_dict(obj.get("order_types"))
        products = Exchanges.from_dict(obj.get("products"))
        user_id = AvatarURL.from_dict(obj.get("user_id"))
        user_name = AvatarURL.from_dict(obj.get("user_name"))
        user_shortname = AvatarURL.from_dict(obj.get("user_shortname"))
        user_type = AvatarURL.from_dict(obj.get("user_type"))
        return DataProperties(avatar_url, broker, email, exchanges, meta, order_types, products, user_id, user_name, user_shortname, user_type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["avatar_url"] = to_class(AvatarURL, self.avatar_url)
        result["broker"] = to_class(AvatarURL, self.broker)
        result["email"] = to_class(AvatarURL, self.email)
        result["exchanges"] = to_class(Exchanges, self.exchanges)
        result["meta"] = to_class(Meta, self.meta)
        result["order_types"] = to_class(Exchanges, self.order_types)
        result["products"] = to_class(Exchanges, self.products)
        result["user_id"] = to_class(AvatarURL, self.user_id)
        result["user_name"] = to_class(AvatarURL, self.user_name)
        result["user_shortname"] = to_class(AvatarURL, self.user_shortname)
        result["user_type"] = to_class(AvatarURL, self.user_type)
        return result


class Data:
    additional_properties: bool
    properties: DataProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: DataProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Data':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = DataProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Data(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(DataProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class MetaProperties:
    demat_consent: AvatarURL

    def __init__(self, demat_consent: AvatarURL) -> None:
        self.demat_consent = demat_consent

    @staticmethod
    def from_dict(obj: Any) -> 'MetaProperties':
        assert isinstance(obj, dict)
        demat_consent = AvatarURL.from_dict(obj.get("demat_consent"))
        return MetaProperties(demat_consent)

    def to_dict(self) -> dict:
        result: dict = {}
        result["demat_consent"] = to_class(AvatarURL, self.demat_consent)
        return result


class MetaClass:
    additional_properties: bool
    properties: MetaProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MetaProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MetaClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MetaProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MetaClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MetaProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class ProfileProperties:
    data: Meta
    status: AvatarURL

    def __init__(self, data: Meta, status: AvatarURL) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'ProfileProperties':
        assert isinstance(obj, dict)
        data = Meta.from_dict(obj.get("data"))
        status = AvatarURL.from_dict(obj.get("status"))
        return ProfileProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Meta, self.data)
        result["status"] = to_class(AvatarURL, self.status)
        return result


class ProfileClass:
    additional_properties: bool
    properties: ProfileProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: ProfileProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'ProfileClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = ProfileProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return ProfileClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(ProfileProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    meta: MetaClass
    profile: ProfileClass

    def __init__(self, data: Data, meta: MetaClass, profile: ProfileClass) -> None:
        self.data = data
        self.meta = meta
        self.profile = profile

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        meta = MetaClass.from_dict(obj.get("Meta"))
        profile = ProfileClass.from_dict(obj.get("Profile"))
        return Definitions(data, meta, profile)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["Meta"] = to_class(MetaClass, self.meta)
        result["Profile"] = to_class(ProfileClass, self.profile)
        return result


class Profile:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'Profile':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return Profile(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def profile_from_dict(s: Any) -> Profile:
    return Profile.from_dict(s)


def profile_to_dict(x: Profile) -> Any:
    return to_class(Profile, x)
