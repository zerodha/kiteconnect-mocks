# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = basket_margins_from_dict(json.loads(json_string))

from typing import Any, List, TypeVar, Type, cast, Callable
from enum import Enum


T = TypeVar("T")
EnumT = TypeVar("EnumT", bound=Enum)


def from_str(x: Any) -> str:
    assert isinstance(x, str)
    return x


def to_enum(c: Type[EnumT], x: Any) -> EnumT:
    assert isinstance(x, c)
    return x.value


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
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'Data':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return Data(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class TypeEnum(Enum):
    INTEGER = "integer"
    NUMBER = "number"
    STRING = "string"


class Status:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Status':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return Status(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class BasketMarginsProperties:
    data: Data
    status: Status

    def __init__(self, data: Data, status: Status) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'BasketMarginsProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = Status.from_dict(obj.get("status"))
        return BasketMarginsProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(Status, self.status)
        return result


class BasketMarginsClass:
    additional_properties: bool
    properties: BasketMarginsProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: BasketMarginsProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'BasketMarginsClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = BasketMarginsProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return BasketMarginsClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(BasketMarginsProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Orders:
    items: Data
    type: str

    def __init__(self, items: Data, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Orders':
        assert isinstance(obj, dict)
        items = Data.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Orders(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(Data, self.items)
        result["type"] = from_str(self.type)
        return result


class DataProperties:
    final: Data
    initial: Data
    orders: Orders

    def __init__(self, final: Data, initial: Data, orders: Orders) -> None:
        self.final = final
        self.initial = initial
        self.orders = orders

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        final = Data.from_dict(obj.get("final"))
        initial = Data.from_dict(obj.get("initial"))
        orders = Orders.from_dict(obj.get("orders"))
        return DataProperties(final, initial, orders)

    def to_dict(self) -> dict:
        result: dict = {}
        result["final"] = to_class(Data, self.final)
        result["initial"] = to_class(Data, self.initial)
        result["orders"] = to_class(Orders, self.orders)
        return result


class DataClass:
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
    def from_dict(obj: Any) -> 'DataClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = DataProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return DataClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(DataProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class FinalProperties:
    additional: Status
    bo: Status
    cash: Status
    exchange: Status
    exposure: Status
    option_premium: Status
    pnl: Data
    span: Status
    total: Status
    tradingsymbol: Status
    type: Status
    var: Status

    def __init__(self, additional: Status, bo: Status, cash: Status, exchange: Status, exposure: Status, option_premium: Status, pnl: Data, span: Status, total: Status, tradingsymbol: Status, type: Status, var: Status) -> None:
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
    def from_dict(obj: Any) -> 'FinalProperties':
        assert isinstance(obj, dict)
        additional = Status.from_dict(obj.get("additional"))
        bo = Status.from_dict(obj.get("bo"))
        cash = Status.from_dict(obj.get("cash"))
        exchange = Status.from_dict(obj.get("exchange"))
        exposure = Status.from_dict(obj.get("exposure"))
        option_premium = Status.from_dict(obj.get("option_premium"))
        pnl = Data.from_dict(obj.get("pnl"))
        span = Status.from_dict(obj.get("span"))
        total = Status.from_dict(obj.get("total"))
        tradingsymbol = Status.from_dict(obj.get("tradingsymbol"))
        type = Status.from_dict(obj.get("type"))
        var = Status.from_dict(obj.get("var"))
        return FinalProperties(additional, bo, cash, exchange, exposure, option_premium, pnl, span, total, tradingsymbol, type, var)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additional"] = to_class(Status, self.additional)
        result["bo"] = to_class(Status, self.bo)
        result["cash"] = to_class(Status, self.cash)
        result["exchange"] = to_class(Status, self.exchange)
        result["exposure"] = to_class(Status, self.exposure)
        result["option_premium"] = to_class(Status, self.option_premium)
        result["pnl"] = to_class(Data, self.pnl)
        result["span"] = to_class(Status, self.span)
        result["total"] = to_class(Status, self.total)
        result["tradingsymbol"] = to_class(Status, self.tradingsymbol)
        result["type"] = to_class(Status, self.type)
        result["var"] = to_class(Status, self.var)
        return result


class Final:
    additional_properties: bool
    properties: FinalProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: FinalProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Final':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = FinalProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Final(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(FinalProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class PnlProperties:
    realised: Status
    unrealised: Status

    def __init__(self, realised: Status, unrealised: Status) -> None:
        self.realised = realised
        self.unrealised = unrealised

    @staticmethod
    def from_dict(obj: Any) -> 'PnlProperties':
        assert isinstance(obj, dict)
        realised = Status.from_dict(obj.get("realised"))
        unrealised = Status.from_dict(obj.get("unrealised"))
        return PnlProperties(realised, unrealised)

    def to_dict(self) -> dict:
        result: dict = {}
        result["realised"] = to_class(Status, self.realised)
        result["unrealised"] = to_class(Status, self.unrealised)
        return result


class Pnl:
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
    def from_dict(obj: Any) -> 'Pnl':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = PnlProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Pnl(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(PnlProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    basket_margins: BasketMarginsClass
    data: DataClass
    final: Final
    pnl: Pnl

    def __init__(self, basket_margins: BasketMarginsClass, data: DataClass, final: Final, pnl: Pnl) -> None:
        self.basket_margins = basket_margins
        self.data = data
        self.final = final
        self.pnl = pnl

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        basket_margins = BasketMarginsClass.from_dict(obj.get("BasketMargins"))
        data = DataClass.from_dict(obj.get("Data"))
        final = Final.from_dict(obj.get("Final"))
        pnl = Pnl.from_dict(obj.get("Pnl"))
        return Definitions(basket_margins, data, final, pnl)

    def to_dict(self) -> dict:
        result: dict = {}
        result["BasketMargins"] = to_class(BasketMarginsClass, self.basket_margins)
        result["Data"] = to_class(DataClass, self.data)
        result["Final"] = to_class(Final, self.final)
        result["Pnl"] = to_class(Pnl, self.pnl)
        return result


class BasketMargins:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'BasketMargins':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return BasketMargins(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def basket_margins_from_dict(s: Any) -> BasketMargins:
    return BasketMargins.from_dict(s)


def basket_margins_to_dict(x: BasketMargins) -> Any:
    return to_class(BasketMargins, x)
