# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = mf_orders_from_dict(json.loads(json_string))

from enum import Enum
from typing import Any, Optional, List, TypeVar, Type, Callable, cast


T = TypeVar("T")
EnumT = TypeVar("EnumT", bound=Enum)


def to_enum(c: Type[EnumT], x: Any) -> EnumT:
    assert isinstance(x, c)
    return x.value


def from_str(x: Any) -> str:
    assert isinstance(x, str)
    return x


def from_none(x: Any) -> Any:
    assert x is None
    return x


def from_union(fs, x):
    for f in fs:
        try:
            return f(x)
        except:
            pass
    assert False


def from_list(f: Callable[[Any], T], x: Any) -> List[T]:
    assert isinstance(x, list)
    return [f(y) for y in x]


def to_class(c: Type[T], x: Any) -> dict:
    assert isinstance(x, c)
    return cast(Any, x).to_dict()


def from_bool(x: Any) -> bool:
    assert isinstance(x, bool)
    return x


class TypeEnum(Enum):
    NULL = "null"
    NUMBER = "number"
    STRING = "string"


class Amount:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Amount':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return Amount(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class LastPriceDate:
    format: Optional[str]
    type: TypeEnum

    def __init__(self, format: Optional[str], type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'LastPriceDate':
        assert isinstance(obj, dict)
        format = from_union([from_str, from_none], obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return LastPriceDate(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_union([from_str, from_none], self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class ExchangeOrderID:
    any_of: List[LastPriceDate]

    def __init__(self, any_of: List[LastPriceDate]) -> None:
        self.any_of = any_of

    @staticmethod
    def from_dict(obj: Any) -> 'ExchangeOrderID':
        assert isinstance(obj, dict)
        any_of = from_list(LastPriceDate.from_dict, obj.get("anyOf"))
        return ExchangeOrderID(any_of)

    def to_dict(self) -> dict:
        result: dict = {}
        result["anyOf"] = from_list(lambda x: to_class(LastPriceDate, x), self.any_of)
        return result


class Tag:
    any_of: List[Amount]

    def __init__(self, any_of: List[Amount]) -> None:
        self.any_of = any_of

    @staticmethod
    def from_dict(obj: Any) -> 'Tag':
        assert isinstance(obj, dict)
        any_of = from_list(Amount.from_dict, obj.get("anyOf"))
        return Tag(any_of)

    def to_dict(self) -> dict:
        result: dict = {}
        result["anyOf"] = from_list(lambda x: to_class(Amount, x), self.any_of)
        return result


class DatumProperties:
    amount: Amount
    average_price: Amount
    exchange_order_id: ExchangeOrderID
    exchange_timestamp: ExchangeOrderID
    folio: Amount
    fund: Amount
    last_price: Amount
    last_price_date: LastPriceDate
    order_id: LastPriceDate
    order_timestamp: LastPriceDate
    placed_by: Amount
    purchase_type: Amount
    quantity: Amount
    settlement_id: ExchangeOrderID
    status: Amount
    status_message: Amount
    tag: Tag
    tradingsymbol: Amount
    transaction_type: Amount
    variety: Amount

    def __init__(self, amount: Amount, average_price: Amount, exchange_order_id: ExchangeOrderID, exchange_timestamp: ExchangeOrderID, folio: Amount, fund: Amount, last_price: Amount, last_price_date: LastPriceDate, order_id: LastPriceDate, order_timestamp: LastPriceDate, placed_by: Amount, purchase_type: Amount, quantity: Amount, settlement_id: ExchangeOrderID, status: Amount, status_message: Amount, tag: Tag, tradingsymbol: Amount, transaction_type: Amount, variety: Amount) -> None:
        self.amount = amount
        self.average_price = average_price
        self.exchange_order_id = exchange_order_id
        self.exchange_timestamp = exchange_timestamp
        self.folio = folio
        self.fund = fund
        self.last_price = last_price
        self.last_price_date = last_price_date
        self.order_id = order_id
        self.order_timestamp = order_timestamp
        self.placed_by = placed_by
        self.purchase_type = purchase_type
        self.quantity = quantity
        self.settlement_id = settlement_id
        self.status = status
        self.status_message = status_message
        self.tag = tag
        self.tradingsymbol = tradingsymbol
        self.transaction_type = transaction_type
        self.variety = variety

    @staticmethod
    def from_dict(obj: Any) -> 'DatumProperties':
        assert isinstance(obj, dict)
        amount = Amount.from_dict(obj.get("amount"))
        average_price = Amount.from_dict(obj.get("average_price"))
        exchange_order_id = ExchangeOrderID.from_dict(obj.get("exchange_order_id"))
        exchange_timestamp = ExchangeOrderID.from_dict(obj.get("exchange_timestamp"))
        folio = Amount.from_dict(obj.get("folio"))
        fund = Amount.from_dict(obj.get("fund"))
        last_price = Amount.from_dict(obj.get("last_price"))
        last_price_date = LastPriceDate.from_dict(obj.get("last_price_date"))
        order_id = LastPriceDate.from_dict(obj.get("order_id"))
        order_timestamp = LastPriceDate.from_dict(obj.get("order_timestamp"))
        placed_by = Amount.from_dict(obj.get("placed_by"))
        purchase_type = Amount.from_dict(obj.get("purchase_type"))
        quantity = Amount.from_dict(obj.get("quantity"))
        settlement_id = ExchangeOrderID.from_dict(obj.get("settlement_id"))
        status = Amount.from_dict(obj.get("status"))
        status_message = Amount.from_dict(obj.get("status_message"))
        tag = Tag.from_dict(obj.get("tag"))
        tradingsymbol = Amount.from_dict(obj.get("tradingsymbol"))
        transaction_type = Amount.from_dict(obj.get("transaction_type"))
        variety = Amount.from_dict(obj.get("variety"))
        return DatumProperties(amount, average_price, exchange_order_id, exchange_timestamp, folio, fund, last_price, last_price_date, order_id, order_timestamp, placed_by, purchase_type, quantity, settlement_id, status, status_message, tag, tradingsymbol, transaction_type, variety)

    def to_dict(self) -> dict:
        result: dict = {}
        result["amount"] = to_class(Amount, self.amount)
        result["average_price"] = to_class(Amount, self.average_price)
        result["exchange_order_id"] = to_class(ExchangeOrderID, self.exchange_order_id)
        result["exchange_timestamp"] = to_class(ExchangeOrderID, self.exchange_timestamp)
        result["folio"] = to_class(Amount, self.folio)
        result["fund"] = to_class(Amount, self.fund)
        result["last_price"] = to_class(Amount, self.last_price)
        result["last_price_date"] = to_class(LastPriceDate, self.last_price_date)
        result["order_id"] = to_class(LastPriceDate, self.order_id)
        result["order_timestamp"] = to_class(LastPriceDate, self.order_timestamp)
        result["placed_by"] = to_class(Amount, self.placed_by)
        result["purchase_type"] = to_class(Amount, self.purchase_type)
        result["quantity"] = to_class(Amount, self.quantity)
        result["settlement_id"] = to_class(ExchangeOrderID, self.settlement_id)
        result["status"] = to_class(Amount, self.status)
        result["status_message"] = to_class(Amount, self.status_message)
        result["tag"] = to_class(Tag, self.tag)
        result["tradingsymbol"] = to_class(Amount, self.tradingsymbol)
        result["transaction_type"] = to_class(Amount, self.transaction_type)
        result["variety"] = to_class(Amount, self.variety)
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


class MFOrdersProperties:
    data: Data
    status: Amount

    def __init__(self, data: Data, status: Amount) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'MFOrdersProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = Amount.from_dict(obj.get("status"))
        return MFOrdersProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(Amount, self.status)
        return result


class MFOrdersClass:
    additional_properties: bool
    properties: MFOrdersProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MFOrdersProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MFOrdersClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MFOrdersProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MFOrdersClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MFOrdersProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    datum: Datum
    mf_orders: MFOrdersClass

    def __init__(self, datum: Datum, mf_orders: MFOrdersClass) -> None:
        self.datum = datum
        self.mf_orders = mf_orders

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        datum = Datum.from_dict(obj.get("Datum"))
        mf_orders = MFOrdersClass.from_dict(obj.get("MFOrders"))
        return Definitions(datum, mf_orders)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Datum"] = to_class(Datum, self.datum)
        result["MFOrders"] = to_class(MFOrdersClass, self.mf_orders)
        return result


class MFOrders:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'MFOrders':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return MFOrders(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def mf_orders_from_dict(s: Any) -> MFOrders:
    return MFOrders.from_dict(s)


def mf_orders_to_dict(x: MFOrders) -> Any:
    return to_class(MFOrders, x)
