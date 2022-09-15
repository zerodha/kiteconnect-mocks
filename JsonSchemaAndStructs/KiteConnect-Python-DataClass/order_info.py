# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = order_info_from_dict(json.loads(json_string))

from enum import Enum
from typing import Any, List, Optional, TypeVar, Type, Callable, cast


T = TypeVar("T")
EnumT = TypeVar("EnumT", bound=Enum)


def to_enum(c: Type[EnumT], x: Any) -> EnumT:
    assert isinstance(x, c)
    return x.value


def from_list(f: Callable[[Any], T], x: Any) -> List[T]:
    assert isinstance(x, list)
    return [f(y) for y in x]


def to_class(c: Type[T], x: Any) -> dict:
    assert isinstance(x, c)
    return cast(Any, x).to_dict()


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


def from_bool(x: Any) -> bool:
    assert isinstance(x, bool)
    return x


class TypeEnum(Enum):
    INTEGER = "integer"
    NULL = "null"
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


class ExchangeOrderID:
    any_of: List[AveragePrice]

    def __init__(self, any_of: List[AveragePrice]) -> None:
        self.any_of = any_of

    @staticmethod
    def from_dict(obj: Any) -> 'ExchangeOrderID':
        assert isinstance(obj, dict)
        any_of = from_list(AveragePrice.from_dict, obj.get("anyOf"))
        return ExchangeOrderID(any_of)

    def to_dict(self) -> dict:
        result: dict = {}
        result["anyOf"] = from_list(lambda x: to_class(AveragePrice, x), self.any_of)
        return result


class OrderTimestamp:
    format: Optional[str]
    type: TypeEnum

    def __init__(self, format: Optional[str], type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'OrderTimestamp':
        assert isinstance(obj, dict)
        format = from_union([from_str, from_none], obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return OrderTimestamp(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_union([from_str, from_none], self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class ExchangeTimestamp:
    any_of: List[OrderTimestamp]

    def __init__(self, any_of: List[OrderTimestamp]) -> None:
        self.any_of = any_of

    @staticmethod
    def from_dict(obj: Any) -> 'ExchangeTimestamp':
        assert isinstance(obj, dict)
        any_of = from_list(OrderTimestamp.from_dict, obj.get("anyOf"))
        return ExchangeTimestamp(any_of)

    def to_dict(self) -> dict:
        result: dict = {}
        result["anyOf"] = from_list(lambda x: to_class(OrderTimestamp, x), self.any_of)
        return result


class DatumProperties:
    average_price: AveragePrice
    cancelled_quantity: AveragePrice
    disclosed_quantity: AveragePrice
    exchange: AveragePrice
    exchange_order_id: ExchangeOrderID
    exchange_timestamp: ExchangeTimestamp
    filled_quantity: AveragePrice
    instrument_token: AveragePrice
    order_id: AveragePrice
    order_timestamp: OrderTimestamp
    order_type: AveragePrice
    parent_order_id: AveragePrice
    pending_quantity: AveragePrice
    placed_by: AveragePrice
    price: AveragePrice
    product: AveragePrice
    quantity: AveragePrice
    status: AveragePrice
    status_message: AveragePrice
    tag: AveragePrice
    tradingsymbol: AveragePrice
    transaction_type: AveragePrice
    trigger_price: AveragePrice
    validity: AveragePrice
    variety: AveragePrice

    def __init__(self, average_price: AveragePrice, cancelled_quantity: AveragePrice, disclosed_quantity: AveragePrice, exchange: AveragePrice, exchange_order_id: ExchangeOrderID, exchange_timestamp: ExchangeTimestamp, filled_quantity: AveragePrice, instrument_token: AveragePrice, order_id: AveragePrice, order_timestamp: OrderTimestamp, order_type: AveragePrice, parent_order_id: AveragePrice, pending_quantity: AveragePrice, placed_by: AveragePrice, price: AveragePrice, product: AveragePrice, quantity: AveragePrice, status: AveragePrice, status_message: AveragePrice, tag: AveragePrice, tradingsymbol: AveragePrice, transaction_type: AveragePrice, trigger_price: AveragePrice, validity: AveragePrice, variety: AveragePrice) -> None:
        self.average_price = average_price
        self.cancelled_quantity = cancelled_quantity
        self.disclosed_quantity = disclosed_quantity
        self.exchange = exchange
        self.exchange_order_id = exchange_order_id
        self.exchange_timestamp = exchange_timestamp
        self.filled_quantity = filled_quantity
        self.instrument_token = instrument_token
        self.order_id = order_id
        self.order_timestamp = order_timestamp
        self.order_type = order_type
        self.parent_order_id = parent_order_id
        self.pending_quantity = pending_quantity
        self.placed_by = placed_by
        self.price = price
        self.product = product
        self.quantity = quantity
        self.status = status
        self.status_message = status_message
        self.tag = tag
        self.tradingsymbol = tradingsymbol
        self.transaction_type = transaction_type
        self.trigger_price = trigger_price
        self.validity = validity
        self.variety = variety

    @staticmethod
    def from_dict(obj: Any) -> 'DatumProperties':
        assert isinstance(obj, dict)
        average_price = AveragePrice.from_dict(obj.get("average_price"))
        cancelled_quantity = AveragePrice.from_dict(obj.get("cancelled_quantity"))
        disclosed_quantity = AveragePrice.from_dict(obj.get("disclosed_quantity"))
        exchange = AveragePrice.from_dict(obj.get("exchange"))
        exchange_order_id = ExchangeOrderID.from_dict(obj.get("exchange_order_id"))
        exchange_timestamp = ExchangeTimestamp.from_dict(obj.get("exchange_timestamp"))
        filled_quantity = AveragePrice.from_dict(obj.get("filled_quantity"))
        instrument_token = AveragePrice.from_dict(obj.get("instrument_token"))
        order_id = AveragePrice.from_dict(obj.get("order_id"))
        order_timestamp = OrderTimestamp.from_dict(obj.get("order_timestamp"))
        order_type = AveragePrice.from_dict(obj.get("order_type"))
        parent_order_id = AveragePrice.from_dict(obj.get("parent_order_id"))
        pending_quantity = AveragePrice.from_dict(obj.get("pending_quantity"))
        placed_by = AveragePrice.from_dict(obj.get("placed_by"))
        price = AveragePrice.from_dict(obj.get("price"))
        product = AveragePrice.from_dict(obj.get("product"))
        quantity = AveragePrice.from_dict(obj.get("quantity"))
        status = AveragePrice.from_dict(obj.get("status"))
        status_message = AveragePrice.from_dict(obj.get("status_message"))
        tag = AveragePrice.from_dict(obj.get("tag"))
        tradingsymbol = AveragePrice.from_dict(obj.get("tradingsymbol"))
        transaction_type = AveragePrice.from_dict(obj.get("transaction_type"))
        trigger_price = AveragePrice.from_dict(obj.get("trigger_price"))
        validity = AveragePrice.from_dict(obj.get("validity"))
        variety = AveragePrice.from_dict(obj.get("variety"))
        return DatumProperties(average_price, cancelled_quantity, disclosed_quantity, exchange, exchange_order_id, exchange_timestamp, filled_quantity, instrument_token, order_id, order_timestamp, order_type, parent_order_id, pending_quantity, placed_by, price, product, quantity, status, status_message, tag, tradingsymbol, transaction_type, trigger_price, validity, variety)

    def to_dict(self) -> dict:
        result: dict = {}
        result["average_price"] = to_class(AveragePrice, self.average_price)
        result["cancelled_quantity"] = to_class(AveragePrice, self.cancelled_quantity)
        result["disclosed_quantity"] = to_class(AveragePrice, self.disclosed_quantity)
        result["exchange"] = to_class(AveragePrice, self.exchange)
        result["exchange_order_id"] = to_class(ExchangeOrderID, self.exchange_order_id)
        result["exchange_timestamp"] = to_class(ExchangeTimestamp, self.exchange_timestamp)
        result["filled_quantity"] = to_class(AveragePrice, self.filled_quantity)
        result["instrument_token"] = to_class(AveragePrice, self.instrument_token)
        result["order_id"] = to_class(AveragePrice, self.order_id)
        result["order_timestamp"] = to_class(OrderTimestamp, self.order_timestamp)
        result["order_type"] = to_class(AveragePrice, self.order_type)
        result["parent_order_id"] = to_class(AveragePrice, self.parent_order_id)
        result["pending_quantity"] = to_class(AveragePrice, self.pending_quantity)
        result["placed_by"] = to_class(AveragePrice, self.placed_by)
        result["price"] = to_class(AveragePrice, self.price)
        result["product"] = to_class(AveragePrice, self.product)
        result["quantity"] = to_class(AveragePrice, self.quantity)
        result["status"] = to_class(AveragePrice, self.status)
        result["status_message"] = to_class(AveragePrice, self.status_message)
        result["tag"] = to_class(AveragePrice, self.tag)
        result["tradingsymbol"] = to_class(AveragePrice, self.tradingsymbol)
        result["transaction_type"] = to_class(AveragePrice, self.transaction_type)
        result["trigger_price"] = to_class(AveragePrice, self.trigger_price)
        result["validity"] = to_class(AveragePrice, self.validity)
        result["variety"] = to_class(AveragePrice, self.variety)
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


class OrderInfoProperties:
    data: Data
    status: AveragePrice

    def __init__(self, data: Data, status: AveragePrice) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'OrderInfoProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = AveragePrice.from_dict(obj.get("status"))
        return OrderInfoProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(AveragePrice, self.status)
        return result


class OrderInfoClass:
    additional_properties: bool
    properties: OrderInfoProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OrderInfoProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'OrderInfoClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OrderInfoProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return OrderInfoClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OrderInfoProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    datum: Datum
    order_info: OrderInfoClass

    def __init__(self, datum: Datum, order_info: OrderInfoClass) -> None:
        self.datum = datum
        self.order_info = order_info

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        datum = Datum.from_dict(obj.get("Datum"))
        order_info = OrderInfoClass.from_dict(obj.get("OrderInfo"))
        return Definitions(datum, order_info)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Datum"] = to_class(Datum, self.datum)
        result["OrderInfo"] = to_class(OrderInfoClass, self.order_info)
        return result


class OrderInfo:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'OrderInfo':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return OrderInfo(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def order_info_from_dict(s: Any) -> OrderInfo:
    return OrderInfo.from_dict(s)


def order_info_to_dict(x: OrderInfo) -> Any:
    return to_class(OrderInfo, x)
