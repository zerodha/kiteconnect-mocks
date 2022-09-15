# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = ohlc_from_dict(json.loads(json_string))

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


class NseInfyProperties:
    instrument_token: InstrumentToken
    last_price: InstrumentToken
    ohlc: NseInfy

    def __init__(self, instrument_token: InstrumentToken, last_price: InstrumentToken, ohlc: NseInfy) -> None:
        self.instrument_token = instrument_token
        self.last_price = last_price
        self.ohlc = ohlc

    @staticmethod
    def from_dict(obj: Any) -> 'NseInfyProperties':
        assert isinstance(obj, dict)
        instrument_token = InstrumentToken.from_dict(obj.get("instrument_token"))
        last_price = InstrumentToken.from_dict(obj.get("last_price"))
        ohlc = NseInfy.from_dict(obj.get("ohlc"))
        return NseInfyProperties(instrument_token, last_price, ohlc)

    def to_dict(self) -> dict:
        result: dict = {}
        result["instrument_token"] = to_class(InstrumentToken, self.instrument_token)
        result["last_price"] = to_class(InstrumentToken, self.last_price)
        result["ohlc"] = to_class(NseInfy, self.ohlc)
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


class OhlcProperties:
    data: NseInfy
    status: InstrumentToken

    def __init__(self, data: NseInfy, status: InstrumentToken) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'OhlcProperties':
        assert isinstance(obj, dict)
        data = NseInfy.from_dict(obj.get("data"))
        status = InstrumentToken.from_dict(obj.get("status"))
        return OhlcProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(NseInfy, self.data)
        result["status"] = to_class(InstrumentToken, self.status)
        return result


class OhlcClass:
    additional_properties: bool
    properties: OhlcProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OhlcProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'OhlcClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OhlcProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return OhlcClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OhlcProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class OhlcClassProperties:
    close: InstrumentToken
    high: InstrumentToken
    low: InstrumentToken
    open: InstrumentToken

    def __init__(self, close: InstrumentToken, high: InstrumentToken, low: InstrumentToken, open: InstrumentToken) -> None:
        self.close = close
        self.high = high
        self.low = low
        self.open = open

    @staticmethod
    def from_dict(obj: Any) -> 'OhlcClassProperties':
        assert isinstance(obj, dict)
        close = InstrumentToken.from_dict(obj.get("close"))
        high = InstrumentToken.from_dict(obj.get("high"))
        low = InstrumentToken.from_dict(obj.get("low"))
        open = InstrumentToken.from_dict(obj.get("open"))
        return OhlcClassProperties(close, high, low, open)

    def to_dict(self) -> dict:
        result: dict = {}
        result["close"] = to_class(InstrumentToken, self.close)
        result["high"] = to_class(InstrumentToken, self.high)
        result["low"] = to_class(InstrumentToken, self.low)
        result["open"] = to_class(InstrumentToken, self.open)
        return result


class OhlcClassClass:
    additional_properties: bool
    properties: OhlcClassProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OhlcClassProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'OhlcClassClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OhlcClassProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return OhlcClassClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OhlcClassProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    nse_infy: NseInfyClass
    ohlc: OhlcClass
    ohlc_class: OhlcClassClass

    def __init__(self, data: Data, nse_infy: NseInfyClass, ohlc: OhlcClass, ohlc_class: OhlcClassClass) -> None:
        self.data = data
        self.nse_infy = nse_infy
        self.ohlc = ohlc
        self.ohlc_class = ohlc_class

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        nse_infy = NseInfyClass.from_dict(obj.get("NseInfy"))
        ohlc = OhlcClass.from_dict(obj.get("Ohlc"))
        ohlc_class = OhlcClassClass.from_dict(obj.get("OhlcClass"))
        return Definitions(data, nse_infy, ohlc, ohlc_class)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["NseInfy"] = to_class(NseInfyClass, self.nse_infy)
        result["Ohlc"] = to_class(OhlcClass, self.ohlc)
        result["OhlcClass"] = to_class(OhlcClassClass, self.ohlc_class)
        return result


class Ohlc:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'Ohlc':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return Ohlc(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def ohlc_from_dict(s: Any) -> Ohlc:
    return Ohlc.from_dict(s)


def ohlc_to_dict(x: Ohlc) -> Any:
    return to_class(Ohlc, x)
