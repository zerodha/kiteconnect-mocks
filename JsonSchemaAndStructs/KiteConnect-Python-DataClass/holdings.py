# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = holdings_from_dict(json.loads(json_string))

from enum import Enum
from typing import Any, List, TypeVar, Type, cast, Callable


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


class TypeEnum(Enum):
    BOOLEAN = "boolean"
    INTEGER = "integer"
    NUMBER = "number"
    STRING = "string"


class AuthorisedDate:
    format: str
    type: TypeEnum

    def __init__(self, format: str, type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'AuthorisedDate':
        assert isinstance(obj, dict)
        format = from_str(obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return AuthorisedDate(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_str(self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class AuthorisedQuantity:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'AuthorisedQuantity':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return AuthorisedQuantity(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class DatumProperties:
    authorised_date: AuthorisedDate
    authorised_quantity: AuthorisedQuantity
    average_price: AuthorisedQuantity
    close_price: AuthorisedQuantity
    collateral_quantity: AuthorisedQuantity
    collateral_type: AuthorisedQuantity
    day_change: AuthorisedQuantity
    day_change_percentage: AuthorisedQuantity
    discrepancy: AuthorisedQuantity
    exchange: AuthorisedQuantity
    instrument_token: AuthorisedQuantity
    isin: AuthorisedQuantity
    last_price: AuthorisedQuantity
    opening_quantity: AuthorisedQuantity
    pnl: AuthorisedQuantity
    price: AuthorisedQuantity
    product: AuthorisedQuantity
    quantity: AuthorisedQuantity
    realised_quantity: AuthorisedQuantity
    t1_quantity: AuthorisedQuantity
    tradingsymbol: AuthorisedQuantity
    used_quantity: AuthorisedQuantity

    def __init__(self, authorised_date: AuthorisedDate, authorised_quantity: AuthorisedQuantity, average_price: AuthorisedQuantity, close_price: AuthorisedQuantity, collateral_quantity: AuthorisedQuantity, collateral_type: AuthorisedQuantity, day_change: AuthorisedQuantity, day_change_percentage: AuthorisedQuantity, discrepancy: AuthorisedQuantity, exchange: AuthorisedQuantity, instrument_token: AuthorisedQuantity, isin: AuthorisedQuantity, last_price: AuthorisedQuantity, opening_quantity: AuthorisedQuantity, pnl: AuthorisedQuantity, price: AuthorisedQuantity, product: AuthorisedQuantity, quantity: AuthorisedQuantity, realised_quantity: AuthorisedQuantity, t1_quantity: AuthorisedQuantity, tradingsymbol: AuthorisedQuantity, used_quantity: AuthorisedQuantity) -> None:
        self.authorised_date = authorised_date
        self.authorised_quantity = authorised_quantity
        self.average_price = average_price
        self.close_price = close_price
        self.collateral_quantity = collateral_quantity
        self.collateral_type = collateral_type
        self.day_change = day_change
        self.day_change_percentage = day_change_percentage
        self.discrepancy = discrepancy
        self.exchange = exchange
        self.instrument_token = instrument_token
        self.isin = isin
        self.last_price = last_price
        self.opening_quantity = opening_quantity
        self.pnl = pnl
        self.price = price
        self.product = product
        self.quantity = quantity
        self.realised_quantity = realised_quantity
        self.t1_quantity = t1_quantity
        self.tradingsymbol = tradingsymbol
        self.used_quantity = used_quantity

    @staticmethod
    def from_dict(obj: Any) -> 'DatumProperties':
        assert isinstance(obj, dict)
        authorised_date = AuthorisedDate.from_dict(obj.get("authorised_date"))
        authorised_quantity = AuthorisedQuantity.from_dict(obj.get("authorised_quantity"))
        average_price = AuthorisedQuantity.from_dict(obj.get("average_price"))
        close_price = AuthorisedQuantity.from_dict(obj.get("close_price"))
        collateral_quantity = AuthorisedQuantity.from_dict(obj.get("collateral_quantity"))
        collateral_type = AuthorisedQuantity.from_dict(obj.get("collateral_type"))
        day_change = AuthorisedQuantity.from_dict(obj.get("day_change"))
        day_change_percentage = AuthorisedQuantity.from_dict(obj.get("day_change_percentage"))
        discrepancy = AuthorisedQuantity.from_dict(obj.get("discrepancy"))
        exchange = AuthorisedQuantity.from_dict(obj.get("exchange"))
        instrument_token = AuthorisedQuantity.from_dict(obj.get("instrument_token"))
        isin = AuthorisedQuantity.from_dict(obj.get("isin"))
        last_price = AuthorisedQuantity.from_dict(obj.get("last_price"))
        opening_quantity = AuthorisedQuantity.from_dict(obj.get("opening_quantity"))
        pnl = AuthorisedQuantity.from_dict(obj.get("pnl"))
        price = AuthorisedQuantity.from_dict(obj.get("price"))
        product = AuthorisedQuantity.from_dict(obj.get("product"))
        quantity = AuthorisedQuantity.from_dict(obj.get("quantity"))
        realised_quantity = AuthorisedQuantity.from_dict(obj.get("realised_quantity"))
        t1_quantity = AuthorisedQuantity.from_dict(obj.get("t1_quantity"))
        tradingsymbol = AuthorisedQuantity.from_dict(obj.get("tradingsymbol"))
        used_quantity = AuthorisedQuantity.from_dict(obj.get("used_quantity"))
        return DatumProperties(authorised_date, authorised_quantity, average_price, close_price, collateral_quantity, collateral_type, day_change, day_change_percentage, discrepancy, exchange, instrument_token, isin, last_price, opening_quantity, pnl, price, product, quantity, realised_quantity, t1_quantity, tradingsymbol, used_quantity)

    def to_dict(self) -> dict:
        result: dict = {}
        result["authorised_date"] = to_class(AuthorisedDate, self.authorised_date)
        result["authorised_quantity"] = to_class(AuthorisedQuantity, self.authorised_quantity)
        result["average_price"] = to_class(AuthorisedQuantity, self.average_price)
        result["close_price"] = to_class(AuthorisedQuantity, self.close_price)
        result["collateral_quantity"] = to_class(AuthorisedQuantity, self.collateral_quantity)
        result["collateral_type"] = to_class(AuthorisedQuantity, self.collateral_type)
        result["day_change"] = to_class(AuthorisedQuantity, self.day_change)
        result["day_change_percentage"] = to_class(AuthorisedQuantity, self.day_change_percentage)
        result["discrepancy"] = to_class(AuthorisedQuantity, self.discrepancy)
        result["exchange"] = to_class(AuthorisedQuantity, self.exchange)
        result["instrument_token"] = to_class(AuthorisedQuantity, self.instrument_token)
        result["isin"] = to_class(AuthorisedQuantity, self.isin)
        result["last_price"] = to_class(AuthorisedQuantity, self.last_price)
        result["opening_quantity"] = to_class(AuthorisedQuantity, self.opening_quantity)
        result["pnl"] = to_class(AuthorisedQuantity, self.pnl)
        result["price"] = to_class(AuthorisedQuantity, self.price)
        result["product"] = to_class(AuthorisedQuantity, self.product)
        result["quantity"] = to_class(AuthorisedQuantity, self.quantity)
        result["realised_quantity"] = to_class(AuthorisedQuantity, self.realised_quantity)
        result["t1_quantity"] = to_class(AuthorisedQuantity, self.t1_quantity)
        result["tradingsymbol"] = to_class(AuthorisedQuantity, self.tradingsymbol)
        result["used_quantity"] = to_class(AuthorisedQuantity, self.used_quantity)
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


class HoldingsProperties:
    data: Data
    status: AuthorisedQuantity

    def __init__(self, data: Data, status: AuthorisedQuantity) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'HoldingsProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = AuthorisedQuantity.from_dict(obj.get("status"))
        return HoldingsProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(AuthorisedQuantity, self.status)
        return result


class HoldingsClass:
    additional_properties: bool
    properties: HoldingsProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: HoldingsProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'HoldingsClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = HoldingsProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return HoldingsClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(HoldingsProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    datum: Datum
    holdings: HoldingsClass

    def __init__(self, datum: Datum, holdings: HoldingsClass) -> None:
        self.datum = datum
        self.holdings = holdings

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        datum = Datum.from_dict(obj.get("Datum"))
        holdings = HoldingsClass.from_dict(obj.get("Holdings"))
        return Definitions(datum, holdings)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Datum"] = to_class(Datum, self.datum)
        result["Holdings"] = to_class(HoldingsClass, self.holdings)
        return result


class Holdings:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'Holdings':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return Holdings(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def holdings_from_dict(s: Any) -> Holdings:
    return Holdings.from_dict(s)


def holdings_to_dict(x: Holdings) -> Any:
    return to_class(Holdings, x)
