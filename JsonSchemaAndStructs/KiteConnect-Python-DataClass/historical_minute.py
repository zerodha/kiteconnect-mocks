# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = historical_minute_from_dict(json.loads(json_string))

from typing import Any, List, TypeVar, Callable, Type, cast


T = TypeVar("T")


def from_str(x: Any) -> str:
    assert isinstance(x, str)
    return x


def from_list(f: Callable[[Any], T], x: Any) -> List[T]:
    assert isinstance(x, list)
    return [f(y) for y in x]


def to_class(c: Type[T], x: Any) -> dict:
    assert isinstance(x, c)
    return cast(Any, x).to_dict()


def from_bool(x: Any) -> bool:
    assert isinstance(x, bool)
    return x


class AnyOf:
    type: str

    def __init__(self, type: str) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'AnyOf':
        assert isinstance(obj, dict)
        type = from_str(obj.get("type"))
        return AnyOf(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = from_str(self.type)
        return result


class Candle:
    any_of: List[AnyOf]
    title: str

    def __init__(self, any_of: List[AnyOf], title: str) -> None:
        self.any_of = any_of
        self.title = title

    @staticmethod
    def from_dict(obj: Any) -> 'Candle':
        assert isinstance(obj, dict)
        any_of = from_list(AnyOf.from_dict, obj.get("anyOf"))
        title = from_str(obj.get("title"))
        return Candle(any_of, title)

    def to_dict(self) -> dict:
        result: dict = {}
        result["anyOf"] = from_list(lambda x: to_class(AnyOf, x), self.any_of)
        result["title"] = from_str(self.title)
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


class Items:
    items: DataClass
    type: str

    def __init__(self, items: DataClass, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Items':
        assert isinstance(obj, dict)
        items = DataClass.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Items(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(DataClass, self.items)
        result["type"] = from_str(self.type)
        return result


class Candles:
    items: Items
    type: str

    def __init__(self, items: Items, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Candles':
        assert isinstance(obj, dict)
        items = Items.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Candles(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(Items, self.items)
        result["type"] = from_str(self.type)
        return result


class DataProperties:
    candles: Candles

    def __init__(self, candles: Candles) -> None:
        self.candles = candles

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        candles = Candles.from_dict(obj.get("candles"))
        return DataProperties(candles)

    def to_dict(self) -> dict:
        result: dict = {}
        result["candles"] = to_class(Candles, self.candles)
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


class HistoricalMinuteProperties:
    data: DataClass
    status: AnyOf

    def __init__(self, data: DataClass, status: AnyOf) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'HistoricalMinuteProperties':
        assert isinstance(obj, dict)
        data = DataClass.from_dict(obj.get("data"))
        status = AnyOf.from_dict(obj.get("status"))
        return HistoricalMinuteProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(DataClass, self.data)
        result["status"] = to_class(AnyOf, self.status)
        return result


class HistoricalMinuteClass:
    additional_properties: bool
    properties: HistoricalMinuteProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: HistoricalMinuteProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'HistoricalMinuteClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = HistoricalMinuteProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return HistoricalMinuteClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(HistoricalMinuteProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    candle: Candle
    data: Data
    historical_minute: HistoricalMinuteClass

    def __init__(self, candle: Candle, data: Data, historical_minute: HistoricalMinuteClass) -> None:
        self.candle = candle
        self.data = data
        self.historical_minute = historical_minute

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        candle = Candle.from_dict(obj.get("Candle"))
        data = Data.from_dict(obj.get("Data"))
        historical_minute = HistoricalMinuteClass.from_dict(obj.get("HistoricalMinute"))
        return Definitions(candle, data, historical_minute)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Candle"] = to_class(Candle, self.candle)
        result["Data"] = to_class(Data, self.data)
        result["HistoricalMinute"] = to_class(HistoricalMinuteClass, self.historical_minute)
        return result


class HistoricalMinute:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'HistoricalMinute':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return HistoricalMinute(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def historical_minute_from_dict(s: Any) -> HistoricalMinute:
    return HistoricalMinute.from_dict(s)


def historical_minute_to_dict(x: HistoricalMinute) -> Any:
    return to_class(HistoricalMinute, x)
