# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = order_margins_from_dict(json.loads(json_string))

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
    INTEGER = "integer"
    NUMBER = "number"
    STRING = "string"


class Additional:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Additional':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return Additional(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class Pnl:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'Pnl':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return Pnl(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class DatumProperties:
    additional: Additional
    bo: Additional
    cash: Additional
    exchange: Additional
    exposure: Additional
    option_premium: Additional
    pnl: Pnl
    span: Additional
    total: Additional
    tradingsymbol: Additional
    type: Additional
    var: Additional

    def __init__(self, additional: Additional, bo: Additional, cash: Additional, exchange: Additional, exposure: Additional, option_premium: Additional, pnl: Pnl, span: Additional, total: Additional, tradingsymbol: Additional, type: Additional, var: Additional) -> None:
        self.additional = additional
        self.bo = bo
        self.cash = cash
        self.exchange = exchange
        self.exposure = exposure
        self.option_premium = option_premium
        self.pnl = pnl
        self.span = span
        self.total = total
        self.tradingsymbol = tradingsymbol
        self.type = type
        self.var = var

    @staticmethod
    def from_dict(obj: Any) -> 'DatumProperties':
        assert isinstance(obj, dict)
        additional = Additional.from_dict(obj.get("additional"))
        bo = Additional.from_dict(obj.get("bo"))
        cash = Additional.from_dict(obj.get("cash"))
        exchange = Additional.from_dict(obj.get("exchange"))
        exposure = Additional.from_dict(obj.get("exposure"))
        option_premium = Additional.from_dict(obj.get("option_premium"))
        pnl = Pnl.from_dict(obj.get("pnl"))
        span = Additional.from_dict(obj.get("span"))
        total = Additional.from_dict(obj.get("total"))
        tradingsymbol = Additional.from_dict(obj.get("tradingsymbol"))
        type = Additional.from_dict(obj.get("type"))
        var = Additional.from_dict(obj.get("var"))
        return DatumProperties(additional, bo, cash, exchange, exposure, option_premium, pnl, span, total, tradingsymbol, type, var)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additional"] = to_class(Additional, self.additional)
        result["bo"] = to_class(Additional, self.bo)
        result["cash"] = to_class(Additional, self.cash)
        result["exchange"] = to_class(Additional, self.exchange)
        result["exposure"] = to_class(Additional, self.exposure)
        result["option_premium"] = to_class(Additional, self.option_premium)
        result["pnl"] = to_class(Pnl, self.pnl)
        result["span"] = to_class(Additional, self.span)
        result["total"] = to_class(Additional, self.total)
        result["tradingsymbol"] = to_class(Additional, self.tradingsymbol)
        result["type"] = to_class(Additional, self.type)
        result["var"] = to_class(Additional, self.var)
        return result


class Datum:
    additional_properties: bool
    properties: DatumProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: DatumProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Datum':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = DatumProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Datum(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(DatumProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Data:
    items: Pnl
    type: str

    def __init__(self, items: Pnl, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Data':
        assert isinstance(obj, dict)
        items = Pnl.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Data(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(Pnl, self.items)
        result["type"] = from_str(self.type)
        return result


class OrderMarginsProperties:
    data: Data
    status: Additional

    def __init__(self, data: Data, status: Additional) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'OrderMarginsProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = Additional.from_dict(obj.get("status"))
        return OrderMarginsProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(Additional, self.status)
        return result


class OrderMarginsClass:
    additional_properties: bool
    properties: OrderMarginsProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OrderMarginsProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'OrderMarginsClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OrderMarginsProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return OrderMarginsClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OrderMarginsProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class PnlProperties:
    realised: Additional
    unrealised: Additional

    def __init__(self, realised: Additional, unrealised: Additional) -> None:
        self.realised = realised
        self.unrealised = unrealised

    @staticmethod
    def from_dict(obj: Any) -> 'PnlProperties':
        assert isinstance(obj, dict)
        realised = Additional.from_dict(obj.get("realised"))
        unrealised = Additional.from_dict(obj.get("unrealised"))
        return PnlProperties(realised, unrealised)

    def to_dict(self) -> dict:
        result: dict = {}
        result["realised"] = to_class(Additional, self.realised)
        result["unrealised"] = to_class(Additional, self.unrealised)
        return result


class PnlClass:
    additional_properties: bool
    properties: PnlProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: PnlProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'PnlClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = PnlProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return PnlClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(PnlProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    datum: Datum
    order_margins: OrderMarginsClass
    pnl: PnlClass

    def __init__(self, datum: Datum, order_margins: OrderMarginsClass, pnl: PnlClass) -> None:
        self.datum = datum
        self.order_margins = order_margins
        self.pnl = pnl

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        datum = Datum.from_dict(obj.get("Datum"))
        order_margins = OrderMarginsClass.from_dict(obj.get("OrderMargins"))
        pnl = PnlClass.from_dict(obj.get("Pnl"))
        return Definitions(datum, order_margins, pnl)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Datum"] = to_class(Datum, self.datum)
        result["OrderMargins"] = to_class(OrderMarginsClass, self.order_margins)
        result["Pnl"] = to_class(PnlClass, self.pnl)
        return result


class OrderMargins:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'OrderMargins':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return OrderMargins(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def order_margins_from_dict(s: Any) -> OrderMargins:
    return OrderMargins.from_dict(s)


def order_margins_to_dict(x: OrderMargins) -> Any:
    return to_class(OrderMargins, x)
