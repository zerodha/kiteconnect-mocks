# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = ltp_from_dict(json.loads(json_string))

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

    def __init__(self, nse_infy: NseInfy) -> None:
        self.nse_infy = nse_infy

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        nse_infy = NseInfy.from_dict(obj.get("NSE:INFY"))
        return DataProperties(nse_infy)

    def to_dict(self) -> dict:
        result: dict = {}
        result["NSE:INFY"] = to_class(NseInfy, self.nse_infy)
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


class Status:
    type: str

    def __init__(self, type: str) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Status':
        assert isinstance(obj, dict)
        type = from_str(obj.get("type"))
        return Status(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = from_str(self.type)
        return result


class LtpProperties:
    data: NseInfy
    status: Status

    def __init__(self, data: NseInfy, status: Status) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'LtpProperties':
        assert isinstance(obj, dict)
        data = NseInfy.from_dict(obj.get("data"))
        status = Status.from_dict(obj.get("status"))
        return LtpProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(NseInfy, self.data)
        result["status"] = to_class(Status, self.status)
        return result


class LtpClass:
    additional_properties: bool
    properties: LtpProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: LtpProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'LtpClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = LtpProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return LtpClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(LtpProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class NseInfyProperties:
    instrument_token: Status
    last_price: Status

    def __init__(self, instrument_token: Status, last_price: Status) -> None:
        self.instrument_token = instrument_token
        self.last_price = last_price

    @staticmethod
    def from_dict(obj: Any) -> 'NseInfyProperties':
        assert isinstance(obj, dict)
        instrument_token = Status.from_dict(obj.get("instrument_token"))
        last_price = Status.from_dict(obj.get("last_price"))
        return NseInfyProperties(instrument_token, last_price)

    def to_dict(self) -> dict:
        result: dict = {}
        result["instrument_token"] = to_class(Status, self.instrument_token)
        result["last_price"] = to_class(Status, self.last_price)
        return result


class NseInfyClass:
    additional_properties: bool
    properties: NseInfyProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: NseInfyProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'NseInfyClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = NseInfyProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return NseInfyClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(NseInfyProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    ltp: LtpClass
    nse_infy: NseInfyClass

    def __init__(self, data: Data, ltp: LtpClass, nse_infy: NseInfyClass) -> None:
        self.data = data
        self.ltp = ltp
        self.nse_infy = nse_infy

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        ltp = LtpClass.from_dict(obj.get("Ltp"))
        nse_infy = NseInfyClass.from_dict(obj.get("NseInfy"))
        return Definitions(data, ltp, nse_infy)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["Ltp"] = to_class(LtpClass, self.ltp)
        result["NseInfy"] = to_class(NseInfyClass, self.nse_infy)
        return result


class Ltp:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'Ltp':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return Ltp(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def ltp_from_dict(s: Any) -> Ltp:
    return Ltp.from_dict(s)


def ltp_to_dict(x: Ltp) -> Any:
    return to_class(Ltp, x)
