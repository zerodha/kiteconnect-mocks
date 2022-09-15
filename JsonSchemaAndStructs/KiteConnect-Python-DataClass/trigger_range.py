# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = trigger_range_from_dict(json.loads(json_string))

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


class NseInfy:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'NseInfy':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return NseInfy(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class DataProperties:
    nse_infy: NseInfy
    nse_reliance: NseInfy

    def __init__(self, nse_infy: NseInfy, nse_reliance: NseInfy) -> None:
        self.nse_infy = nse_infy
        self.nse_reliance = nse_reliance

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        nse_infy = NseInfy.from_dict(obj.get("NSE:INFY"))
        nse_reliance = NseInfy.from_dict(obj.get("NSE:RELIANCE"))
        return DataProperties(nse_infy, nse_reliance)

    def to_dict(self) -> dict:
        result: dict = {}
        result["NSE:INFY"] = to_class(NseInfy, self.nse_infy)
        result["NSE:RELIANCE"] = to_class(NseInfy, self.nse_reliance)
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


class InstrumentToken:
    type: str

    def __init__(self, type: str) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'InstrumentToken':
        assert isinstance(obj, dict)
        type = from_str(obj.get("type"))
        return InstrumentToken(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = from_str(self.type)
        return result


class NseProperties:
    instrument_token: InstrumentToken
    lower: InstrumentToken
    upper: InstrumentToken

    def __init__(self, instrument_token: InstrumentToken, lower: InstrumentToken, upper: InstrumentToken) -> None:
        self.instrument_token = instrument_token
        self.lower = lower
        self.upper = upper

    @staticmethod
    def from_dict(obj: Any) -> 'NseProperties':
        assert isinstance(obj, dict)
        instrument_token = InstrumentToken.from_dict(obj.get("instrument_token"))
        lower = InstrumentToken.from_dict(obj.get("lower"))
        upper = InstrumentToken.from_dict(obj.get("upper"))
        return NseProperties(instrument_token, lower, upper)

    def to_dict(self) -> dict:
        result: dict = {}
        result["instrument_token"] = to_class(InstrumentToken, self.instrument_token)
        result["lower"] = to_class(InstrumentToken, self.lower)
        result["upper"] = to_class(InstrumentToken, self.upper)
        return result


class Nse:
    additional_properties: bool
    properties: NseProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: NseProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Nse':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = NseProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Nse(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(NseProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class TriggerRangeProperties:
    data: NseInfy
    status: InstrumentToken

    def __init__(self, data: NseInfy, status: InstrumentToken) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'TriggerRangeProperties':
        assert isinstance(obj, dict)
        data = NseInfy.from_dict(obj.get("data"))
        status = InstrumentToken.from_dict(obj.get("status"))
        return TriggerRangeProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(NseInfy, self.data)
        result["status"] = to_class(InstrumentToken, self.status)
        return result


class TriggerRangeClass:
    additional_properties: bool
    properties: TriggerRangeProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: TriggerRangeProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'TriggerRangeClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = TriggerRangeProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return TriggerRangeClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(TriggerRangeProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    nse: Nse
    trigger_range: TriggerRangeClass

    def __init__(self, data: Data, nse: Nse, trigger_range: TriggerRangeClass) -> None:
        self.data = data
        self.nse = nse
        self.trigger_range = trigger_range

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        nse = Nse.from_dict(obj.get("Nse"))
        trigger_range = TriggerRangeClass.from_dict(obj.get("TriggerRange"))
        return Definitions(data, nse, trigger_range)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["Nse"] = to_class(Nse, self.nse)
        result["TriggerRange"] = to_class(TriggerRangeClass, self.trigger_range)
        return result


class TriggerRange:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'TriggerRange':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return TriggerRange(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def trigger_range_from_dict(s: Any) -> TriggerRange:
    return TriggerRange.from_dict(s)


def trigger_range_to_dict(x: TriggerRange) -> Any:
    return to_class(TriggerRange, x)
