# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = convert_position_from_dict(json.loads(json_string))

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


class Data:
    type: str

    def __init__(self, type: str) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Data':
        assert isinstance(obj, dict)
        type = from_str(obj.get("type"))
        return Data(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = from_str(self.type)
        return result


class Properties:
    data: Data
    status: Data

    def __init__(self, data: Data, status: Data) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'Properties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = Data.from_dict(obj.get("status"))
        return Properties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(Data, self.status)
        return result


class ConvertPositionClass:
    additional_properties: bool
    properties: Properties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: Properties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'ConvertPositionClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = Properties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return ConvertPositionClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(Properties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    convert_position: ConvertPositionClass

    def __init__(self, convert_position: ConvertPositionClass) -> None:
        self.convert_position = convert_position

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        convert_position = ConvertPositionClass.from_dict(obj.get("ConvertPosition"))
        return Definitions(convert_position)

    def to_dict(self) -> dict:
        result: dict = {}
        result["ConvertPosition"] = to_class(ConvertPositionClass, self.convert_position)
        return result


class ConvertPosition:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'ConvertPosition':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return ConvertPosition(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def convert_position_from_dict(s: Any) -> ConvertPosition:
    return ConvertPosition.from_dict(s)


def convert_position_to_dict(x: ConvertPosition) -> Any:
    return to_class(ConvertPosition, x)
