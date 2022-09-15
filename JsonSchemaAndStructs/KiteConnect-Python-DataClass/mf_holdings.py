# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = mf_holdings_from_dict(json.loads(json_string))

from enum import Enum
from typing import Any, List, TypeVar, Type, cast, Callable


T = TypeVar("T")
EnumT = TypeVar("EnumT", bound=Enum)


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


def from_str(x: Any) -> str:
    assert isinstance(x, str)
    return x


class TypeEnum(Enum):
    INTEGER = "integer"
    NUMBER = "number"
    STRING = "string"


class AveragePrice:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'AveragePrice':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return AveragePrice(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class DatumProperties:
    average_price: AveragePrice
    folio: AveragePrice
    fund: AveragePrice
    last_price: AveragePrice
    last_price_date: AveragePrice
    pledged_quantity: AveragePrice
    pnl: AveragePrice
    quantity: AveragePrice
    tradingsymbol: AveragePrice

    def __init__(self, average_price: AveragePrice, folio: AveragePrice, fund: AveragePrice, last_price: AveragePrice, last_price_date: AveragePrice, pledged_quantity: AveragePrice, pnl: AveragePrice, quantity: AveragePrice, tradingsymbol: AveragePrice) -> None:
        self.average_price = average_price
        self.folio = folio
        self.fund = fund
        self.last_price = last_price
        self.last_price_date = last_price_date
        self.pledged_quantity = pledged_quantity
        self.pnl = pnl
        self.quantity = quantity
        self.tradingsymbol = tradingsymbol

    @staticmethod
    def from_dict(obj: Any) -> 'DatumProperties':
        assert isinstance(obj, dict)
        average_price = AveragePrice.from_dict(obj.get("average_price"))
        folio = AveragePrice.from_dict(obj.get("folio"))
        fund = AveragePrice.from_dict(obj.get("fund"))
        last_price = AveragePrice.from_dict(obj.get("last_price"))
        last_price_date = AveragePrice.from_dict(obj.get("last_price_date"))
        pledged_quantity = AveragePrice.from_dict(obj.get("pledged_quantity"))
        pnl = AveragePrice.from_dict(obj.get("pnl"))
        quantity = AveragePrice.from_dict(obj.get("quantity"))
        tradingsymbol = AveragePrice.from_dict(obj.get("tradingsymbol"))
        return DatumProperties(average_price, folio, fund, last_price, last_price_date, pledged_quantity, pnl, quantity, tradingsymbol)

    def to_dict(self) -> dict:
        result: dict = {}
        result["average_price"] = to_class(AveragePrice, self.average_price)
        result["folio"] = to_class(AveragePrice, self.folio)
        result["fund"] = to_class(AveragePrice, self.fund)
        result["last_price"] = to_class(AveragePrice, self.last_price)
        result["last_price_date"] = to_class(AveragePrice, self.last_price_date)
        result["pledged_quantity"] = to_class(AveragePrice, self.pledged_quantity)
        result["pnl"] = to_class(AveragePrice, self.pnl)
        result["quantity"] = to_class(AveragePrice, self.quantity)
        result["tradingsymbol"] = to_class(AveragePrice, self.tradingsymbol)
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


class Items:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'Items':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return Items(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class Data:
    items: Items
    type: str

    def __init__(self, items: Items, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Data':
        assert isinstance(obj, dict)
        items = Items.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Data(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(Items, self.items)
        result["type"] = from_str(self.type)
        return result


class MFHoldingsProperties:
    data: Data
    status: AveragePrice

    def __init__(self, data: Data, status: AveragePrice) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'MFHoldingsProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = AveragePrice.from_dict(obj.get("status"))
        return MFHoldingsProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(AveragePrice, self.status)
        return result


class MFHoldingsClass:
    additional_properties: bool
    properties: MFHoldingsProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MFHoldingsProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MFHoldingsClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MFHoldingsProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MFHoldingsClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MFHoldingsProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    datum: Datum
    mf_holdings: MFHoldingsClass

    def __init__(self, datum: Datum, mf_holdings: MFHoldingsClass) -> None:
        self.datum = datum
        self.mf_holdings = mf_holdings

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        datum = Datum.from_dict(obj.get("Datum"))
        mf_holdings = MFHoldingsClass.from_dict(obj.get("MFHoldings"))
        return Definitions(datum, mf_holdings)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Datum"] = to_class(Datum, self.datum)
        result["MFHoldings"] = to_class(MFHoldingsClass, self.mf_holdings)
        return result


class MFHoldings:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'MFHoldings':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return MFHoldings(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def mf_holdings_from_dict(s: Any) -> MFHoldings:
    return MFHoldings.from_dict(s)


def mf_holdings_to_dict(x: MFHoldings) -> Any:
    return to_class(MFHoldings, x)
