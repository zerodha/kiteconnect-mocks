# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = mf_orders_info_from_dict(json.loads(json_string))

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
    format: str
    type: TypeEnum

    def __init__(self, format: str, type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'LastPriceDate':
        assert isinstance(obj, dict)
        format = from_str(obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return LastPriceDate(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_str(self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class DataProperties:
    amount: Amount
    average_price: Amount
    exchange_order_id: Amount
    exchange_timestamp: Amount
    folio: Amount
    fund: Amount
    last_price: Amount
    last_price_date: LastPriceDate
    order_id: LastPriceDate
    order_timestamp: LastPriceDate
    placed_by: Amount
    purchase_type: Amount
    quantity: Amount
    settlement_id: Amount
    status: Amount
    status_message: Amount
    tag: Amount
    tradingsymbol: Amount
    transaction_type: Amount
    variety: Amount

    def __init__(self, amount: Amount, average_price: Amount, exchange_order_id: Amount, exchange_timestamp: Amount, folio: Amount, fund: Amount, last_price: Amount, last_price_date: LastPriceDate, order_id: LastPriceDate, order_timestamp: LastPriceDate, placed_by: Amount, purchase_type: Amount, quantity: Amount, settlement_id: Amount, status: Amount, status_message: Amount, tag: Amount, tradingsymbol: Amount, transaction_type: Amount, variety: Amount) -> None:
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
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        amount = Amount.from_dict(obj.get("amount"))
        average_price = Amount.from_dict(obj.get("average_price"))
        exchange_order_id = Amount.from_dict(obj.get("exchange_order_id"))
        exchange_timestamp = Amount.from_dict(obj.get("exchange_timestamp"))
        folio = Amount.from_dict(obj.get("folio"))
        fund = Amount.from_dict(obj.get("fund"))
        last_price = Amount.from_dict(obj.get("last_price"))
        last_price_date = LastPriceDate.from_dict(obj.get("last_price_date"))
        order_id = LastPriceDate.from_dict(obj.get("order_id"))
        order_timestamp = LastPriceDate.from_dict(obj.get("order_timestamp"))
        placed_by = Amount.from_dict(obj.get("placed_by"))
        purchase_type = Amount.from_dict(obj.get("purchase_type"))
        quantity = Amount.from_dict(obj.get("quantity"))
        settlement_id = Amount.from_dict(obj.get("settlement_id"))
        status = Amount.from_dict(obj.get("status"))
        status_message = Amount.from_dict(obj.get("status_message"))
        tag = Amount.from_dict(obj.get("tag"))
        tradingsymbol = Amount.from_dict(obj.get("tradingsymbol"))
        transaction_type = Amount.from_dict(obj.get("transaction_type"))
        variety = Amount.from_dict(obj.get("variety"))
        return DataProperties(amount, average_price, exchange_order_id, exchange_timestamp, folio, fund, last_price, last_price_date, order_id, order_timestamp, placed_by, purchase_type, quantity, settlement_id, status, status_message, tag, tradingsymbol, transaction_type, variety)

    def to_dict(self) -> dict:
        result: dict = {}
        result["amount"] = to_class(Amount, self.amount)
        result["average_price"] = to_class(Amount, self.average_price)
        result["exchange_order_id"] = to_class(Amount, self.exchange_order_id)
        result["exchange_timestamp"] = to_class(Amount, self.exchange_timestamp)
        result["folio"] = to_class(Amount, self.folio)
        result["fund"] = to_class(Amount, self.fund)
        result["last_price"] = to_class(Amount, self.last_price)
        result["last_price_date"] = to_class(LastPriceDate, self.last_price_date)
        result["order_id"] = to_class(LastPriceDate, self.order_id)
        result["order_timestamp"] = to_class(LastPriceDate, self.order_timestamp)
        result["placed_by"] = to_class(Amount, self.placed_by)
        result["purchase_type"] = to_class(Amount, self.purchase_type)
        result["quantity"] = to_class(Amount, self.quantity)
        result["settlement_id"] = to_class(Amount, self.settlement_id)
        result["status"] = to_class(Amount, self.status)
        result["status_message"] = to_class(Amount, self.status_message)
        result["tag"] = to_class(Amount, self.tag)
        result["tradingsymbol"] = to_class(Amount, self.tradingsymbol)
        result["transaction_type"] = to_class(Amount, self.transaction_type)
        result["variety"] = to_class(Amount, self.variety)
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


class MFOrdersInfoProperties:
    data: DataClass
    status: Amount

    def __init__(self, data: DataClass, status: Amount) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'MFOrdersInfoProperties':
        assert isinstance(obj, dict)
        data = DataClass.from_dict(obj.get("data"))
        status = Amount.from_dict(obj.get("status"))
        return MFOrdersInfoProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(DataClass, self.data)
        result["status"] = to_class(Amount, self.status)
        return result


class MFOrdersInfoClass:
    additional_properties: bool
    properties: MFOrdersInfoProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MFOrdersInfoProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MFOrdersInfoClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MFOrdersInfoProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MFOrdersInfoClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MFOrdersInfoProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    mf_orders_info: MFOrdersInfoClass

    def __init__(self, data: Data, mf_orders_info: MFOrdersInfoClass) -> None:
        self.data = data
        self.mf_orders_info = mf_orders_info

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        mf_orders_info = MFOrdersInfoClass.from_dict(obj.get("MFOrdersInfo"))
        return Definitions(data, mf_orders_info)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["MFOrdersInfo"] = to_class(MFOrdersInfoClass, self.mf_orders_info)
        return result


class MFOrdersInfo:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'MFOrdersInfo':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return MFOrdersInfo(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def mf_orders_info_from_dict(s: Any) -> MFOrdersInfo:
    return MFOrdersInfo.from_dict(s)


def mf_orders_info_to_dict(x: MFOrdersInfo) -> Any:
    return to_class(MFOrdersInfo, x)
