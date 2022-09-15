# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = holdings_auth_from_dict(json.loads(json_string))

from typing import Any, List, TypeVar, Type, cast, Callable


T = TypeVar("T")


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


class RequestID:
    type: str

    def __init__(self, type: str) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'RequestID':
        assert isinstance(obj, dict)
        type = from_str(obj.get("type"))
        return RequestID(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = from_str(self.type)
        return result


class DataProperties:
    request_id: RequestID

    def __init__(self, request_id: RequestID) -> None:
        self.request_id = request_id

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        request_id = RequestID.from_dict(obj.get("request_id"))
        return DataProperties(request_id)

    def to_dict(self) -> dict:
        result: dict = {}
        result["request_id"] = to_class(RequestID, self.request_id)
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


class DataClass:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'DataClass':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return DataClass(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class HoldingsAuthProperties:
    data: DataClass
    status: RequestID

    def __init__(self, data: DataClass, status: RequestID) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'HoldingsAuthProperties':
        assert isinstance(obj, dict)
        data = DataClass.from_dict(obj.get("data"))
        status = RequestID.from_dict(obj.get("status"))
        return HoldingsAuthProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(DataClass, self.data)
        result["status"] = to_class(RequestID, self.status)
        return result


class HoldingsAuthClass:
    additional_properties: bool
    properties: HoldingsAuthProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: HoldingsAuthProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'HoldingsAuthClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = HoldingsAuthProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return HoldingsAuthClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(HoldingsAuthProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    holdings_auth: HoldingsAuthClass

    def __init__(self, data: Data, holdings_auth: HoldingsAuthClass) -> None:
        self.data = data
        self.holdings_auth = holdings_auth

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        holdings_auth = HoldingsAuthClass.from_dict(obj.get("HoldingsAuth"))
        return Definitions(data, holdings_auth)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["HoldingsAuth"] = to_class(HoldingsAuthClass, self.holdings_auth)
        return result


class HoldingsAuth:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'HoldingsAuth':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return HoldingsAuth(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def holdings_auth_from_dict(s: Any) -> HoldingsAuth:
    return HoldingsAuth.from_dict(s)


def holdings_auth_to_dict(x: HoldingsAuth) -> Any:
    return to_class(HoldingsAuth, x)
