# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = gtt_place_order_from_dict(json.loads(json_string))

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


class TriggerID:
    type: str

    def __init__(self, type: str) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'TriggerID':
        assert isinstance(obj, dict)
        type = from_str(obj.get("type"))
        return TriggerID(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = from_str(self.type)
        return result


class DataProperties:
    trigger_id: TriggerID

    def __init__(self, trigger_id: TriggerID) -> None:
        self.trigger_id = trigger_id

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        trigger_id = TriggerID.from_dict(obj.get("trigger_id"))
        return DataProperties(trigger_id)

    def to_dict(self) -> dict:
        result: dict = {}
        result["trigger_id"] = to_class(TriggerID, self.trigger_id)
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


class GttPlaceOrderProperties:
    data: DataClass
    status: TriggerID

    def __init__(self, data: DataClass, status: TriggerID) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'GttPlaceOrderProperties':
        assert isinstance(obj, dict)
        data = DataClass.from_dict(obj.get("data"))
        status = TriggerID.from_dict(obj.get("status"))
        return GttPlaceOrderProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(DataClass, self.data)
        result["status"] = to_class(TriggerID, self.status)
        return result


class GttPlaceOrderClass:
    additional_properties: bool
    properties: GttPlaceOrderProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: GttPlaceOrderProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'GttPlaceOrderClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = GttPlaceOrderProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return GttPlaceOrderClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(GttPlaceOrderProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    gtt_place_order: GttPlaceOrderClass

    def __init__(self, data: Data, gtt_place_order: GttPlaceOrderClass) -> None:
        self.data = data
        self.gtt_place_order = gtt_place_order

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        gtt_place_order = GttPlaceOrderClass.from_dict(obj.get("GttPlaceOrder"))
        return Definitions(data, gtt_place_order)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["GttPlaceOrder"] = to_class(GttPlaceOrderClass, self.gtt_place_order)
        return result


class GttPlaceOrder:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'GttPlaceOrder':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return GttPlaceOrder(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def gtt_place_order_from_dict(s: Any) -> GttPlaceOrder:
    return GttPlaceOrder.from_dict(s)


def gtt_place_order_to_dict(x: GttPlaceOrder) -> Any:
    return to_class(GttPlaceOrder, x)