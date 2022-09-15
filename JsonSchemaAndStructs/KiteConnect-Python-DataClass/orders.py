# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = orders_from_dict(json.loads(json_string))

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
    BOOLEAN = "boolean"
    INTEGER = "integer"
    NULL = "null"
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


class ExchangeETimestamp:
    any_of: List[OrderTimestamp]

    def __init__(self, any_of: List[OrderTimestamp]) -> None:
        self.any_of = any_of

    @staticmethod
    def from_dict(obj: Any) -> 'ExchangeETimestamp':
        assert isinstance(obj, dict)
        any_of = from_list(OrderTimestamp.from_dict, obj.get("anyOf"))
        return ExchangeETimestamp(any_of)

    def to_dict(self) -> dict:
        result: dict = {}
        result["anyOf"] = from_list(lambda x: to_class(OrderTimestamp, x), self.any_of)
        return result


class Meta:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'Meta':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return Meta(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class Tags:
    items: AveragePrice
    type: str

    def __init__(self, items: AveragePrice, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Tags':
        assert isinstance(obj, dict)
        items = AveragePrice.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Tags(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(AveragePrice, self.items)
        result["type"] = from_str(self.type)
        return result


class DatumProperties:
    average_price: AveragePrice
    cancelled_quantity: AveragePrice
    disclosed_quantity: AveragePrice
    exchange: AveragePrice
    exchange_order_id: ExchangeOrderID
    exchange_timestamp: ExchangeETimestamp
    exchange_update_timestamp: ExchangeETimestamp
    filled_quantity: AveragePrice
    guid: AveragePrice
    instrument_token: AveragePrice
    market_protection: AveragePrice
    meta: Meta
    modified: AveragePrice
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
    status_message: ExchangeOrderID
    status_message_raw: ExchangeOrderID
    tag: ExchangeOrderID
    tags: Tags
    tradingsymbol: AveragePrice
    transaction_type: AveragePrice
    trigger_price: AveragePrice
    validity: AveragePrice
    validity_ttl: AveragePrice
    variety: AveragePrice

    def __init__(self, average_price: AveragePrice, cancelled_quantity: AveragePrice, disclosed_quantity: AveragePrice, exchange: AveragePrice, exchange_order_id: ExchangeOrderID, exchange_timestamp: ExchangeETimestamp, exchange_update_timestamp: ExchangeETimestamp, filled_quantity: AveragePrice, guid: AveragePrice, instrument_token: AveragePrice, market_protection: AveragePrice, meta: Meta, modified: AveragePrice, order_id: AveragePrice, order_timestamp: OrderTimestamp, order_type: AveragePrice, parent_order_id: AveragePrice, pending_quantity: AveragePrice, placed_by: AveragePrice, price: AveragePrice, product: AveragePrice, quantity: AveragePrice, status: AveragePrice, status_message: ExchangeOrderID, status_message_raw: ExchangeOrderID, tag: ExchangeOrderID, tags: Tags, tradingsymbol: AveragePrice, transaction_type: AveragePrice, trigger_price: AveragePrice, validity: AveragePrice, validity_ttl: AveragePrice, variety: AveragePrice) -> None:
        self.average_price = average_price
        self.cancelled_quantity = cancelled_quantity
        self.disclosed_quantity = disclosed_quantity
        self.exchange = exchange
        self.exchange_order_id = exchange_order_id
        self.exchange_timestamp = exchange_timestamp
        self.exchange_update_timestamp = exchange_update_timestamp
        self.filled_quantity = filled_quantity
        self.guid = guid
        self.instrument_token = instrument_token
        self.market_protection = market_protection
        self.meta = meta
        self.modified = modified
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
        self.status_message_raw = status_message_raw
        self.tag = tag
        self.tags = tags
        self.tradingsymbol = tradingsymbol
        self.transaction_type = transaction_type
        self.trigger_price = trigger_price
        self.validity = validity
        self.validity_ttl = validity_ttl
        self.variety = variety

    @staticmethod
    def from_dict(obj: Any) -> 'DatumProperties':
        assert isinstance(obj, dict)
        average_price = AveragePrice.from_dict(obj.get("average_price"))
        cancelled_quantity = AveragePrice.from_dict(obj.get("cancelled_quantity"))
        disclosed_quantity = AveragePrice.from_dict(obj.get("disclosed_quantity"))
        exchange = AveragePrice.from_dict(obj.get("exchange"))
        exchange_order_id = ExchangeOrderID.from_dict(obj.get("exchange_order_id"))
        exchange_timestamp = ExchangeETimestamp.from_dict(obj.get("exchange_timestamp"))
        exchange_update_timestamp = ExchangeETimestamp.from_dict(obj.get("exchange_update_timestamp"))
        filled_quantity = AveragePrice.from_dict(obj.get("filled_quantity"))
        guid = AveragePrice.from_dict(obj.get("guid"))
        instrument_token = AveragePrice.from_dict(obj.get("instrument_token"))
        market_protection = AveragePrice.from_dict(obj.get("market_protection"))
        meta = Meta.from_dict(obj.get("meta"))
        modified = AveragePrice.from_dict(obj.get("modified"))
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
        status_message = ExchangeOrderID.from_dict(obj.get("status_message"))
        status_message_raw = ExchangeOrderID.from_dict(obj.get("status_message_raw"))
        tag = ExchangeOrderID.from_dict(obj.get("tag"))
        tags = Tags.from_dict(obj.get("tags"))
        tradingsymbol = AveragePrice.from_dict(obj.get("tradingsymbol"))
        transaction_type = AveragePrice.from_dict(obj.get("transaction_type"))
        trigger_price = AveragePrice.from_dict(obj.get("trigger_price"))
        validity = AveragePrice.from_dict(obj.get("validity"))
        validity_ttl = AveragePrice.from_dict(obj.get("validity_ttl"))
        variety = AveragePrice.from_dict(obj.get("variety"))
        return DatumProperties(average_price, cancelled_quantity, disclosed_quantity, exchange, exchange_order_id, exchange_timestamp, exchange_update_timestamp, filled_quantity, guid, instrument_token, market_protection, meta, modified, order_id, order_timestamp, order_type, parent_order_id, pending_quantity, placed_by, price, product, quantity, status, status_message, status_message_raw, tag, tags, tradingsymbol, transaction_type, trigger_price, validity, validity_ttl, variety)

    def to_dict(self) -> dict:
        result: dict = {}
        result["average_price"] = to_class(AveragePrice, self.average_price)
        result["cancelled_quantity"] = to_class(AveragePrice, self.cancelled_quantity)
        result["disclosed_quantity"] = to_class(AveragePrice, self.disclosed_quantity)
        result["exchange"] = to_class(AveragePrice, self.exchange)
        result["exchange_order_id"] = to_class(ExchangeOrderID, self.exchange_order_id)
        result["exchange_timestamp"] = to_class(ExchangeETimestamp, self.exchange_timestamp)
        result["exchange_update_timestamp"] = to_class(ExchangeETimestamp, self.exchange_update_timestamp)
        result["filled_quantity"] = to_class(AveragePrice, self.filled_quantity)
        result["guid"] = to_class(AveragePrice, self.guid)
        result["instrument_token"] = to_class(AveragePrice, self.instrument_token)
        result["market_protection"] = to_class(AveragePrice, self.market_protection)
        result["meta"] = to_class(Meta, self.meta)
        result["modified"] = to_class(AveragePrice, self.modified)
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
        result["status_message"] = to_class(ExchangeOrderID, self.status_message)
        result["status_message_raw"] = to_class(ExchangeOrderID, self.status_message_raw)
        result["tag"] = to_class(ExchangeOrderID, self.tag)
        result["tags"] = to_class(Tags, self.tags)
        result["tradingsymbol"] = to_class(AveragePrice, self.tradingsymbol)
        result["transaction_type"] = to_class(AveragePrice, self.transaction_type)
        result["trigger_price"] = to_class(AveragePrice, self.trigger_price)
        result["validity"] = to_class(AveragePrice, self.validity)
        result["validity_ttl"] = to_class(AveragePrice, self.validity_ttl)
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


class IcebergProperties:
    leg: AveragePrice
    leg_quantity: AveragePrice
    legs: AveragePrice
    remaining_quantity: AveragePrice
    total_quantity: AveragePrice

    def __init__(self, leg: AveragePrice, leg_quantity: AveragePrice, legs: AveragePrice, remaining_quantity: AveragePrice, total_quantity: AveragePrice) -> None:
        self.leg = leg
        self.leg_quantity = leg_quantity
        self.legs = legs
        self.remaining_quantity = remaining_quantity
        self.total_quantity = total_quantity

    @staticmethod
    def from_dict(obj: Any) -> 'IcebergProperties':
        assert isinstance(obj, dict)
        leg = AveragePrice.from_dict(obj.get("leg"))
        leg_quantity = AveragePrice.from_dict(obj.get("leg_quantity"))
        legs = AveragePrice.from_dict(obj.get("legs"))
        remaining_quantity = AveragePrice.from_dict(obj.get("remaining_quantity"))
        total_quantity = AveragePrice.from_dict(obj.get("total_quantity"))
        return IcebergProperties(leg, leg_quantity, legs, remaining_quantity, total_quantity)

    def to_dict(self) -> dict:
        result: dict = {}
        result["leg"] = to_class(AveragePrice, self.leg)
        result["leg_quantity"] = to_class(AveragePrice, self.leg_quantity)
        result["legs"] = to_class(AveragePrice, self.legs)
        result["remaining_quantity"] = to_class(AveragePrice, self.remaining_quantity)
        result["total_quantity"] = to_class(AveragePrice, self.total_quantity)
        return result


class Iceberg:
    additional_properties: bool
    properties: IcebergProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: IcebergProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Iceberg':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = IcebergProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Iceberg(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(IcebergProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class MetaProperties:
    iceberg: Meta

    def __init__(self, iceberg: Meta) -> None:
        self.iceberg = iceberg

    @staticmethod
    def from_dict(obj: Any) -> 'MetaProperties':
        assert isinstance(obj, dict)
        iceberg = Meta.from_dict(obj.get("iceberg"))
        return MetaProperties(iceberg)

    def to_dict(self) -> dict:
        result: dict = {}
        result["iceberg"] = to_class(Meta, self.iceberg)
        return result


class MetaClass:
    additional_properties: bool
    properties: MetaProperties
    required: List[Any]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MetaProperties, required: List[Any], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MetaClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MetaProperties.from_dict(obj.get("properties"))
        required = from_list(lambda x: x, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MetaClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MetaProperties, self.properties)
        result["required"] = from_list(lambda x: x, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Data:
    items: Meta
    type: str

    def __init__(self, items: Meta, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Data':
        assert isinstance(obj, dict)
        items = Meta.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Data(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(Meta, self.items)
        result["type"] = from_str(self.type)
        return result


class OrdersProperties:
    data: Data
    status: AveragePrice

    def __init__(self, data: Data, status: AveragePrice) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'OrdersProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = AveragePrice.from_dict(obj.get("status"))
        return OrdersProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(AveragePrice, self.status)
        return result


class OrdersClass:
    additional_properties: bool
    properties: OrdersProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OrdersProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'OrdersClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OrdersProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return OrdersClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OrdersProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    datum: Datum
    iceberg: Iceberg
    meta: MetaClass
    orders: OrdersClass

    def __init__(self, datum: Datum, iceberg: Iceberg, meta: MetaClass, orders: OrdersClass) -> None:
        self.datum = datum
        self.iceberg = iceberg
        self.meta = meta
        self.orders = orders

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        datum = Datum.from_dict(obj.get("Datum"))
        iceberg = Iceberg.from_dict(obj.get("Iceberg"))
        meta = MetaClass.from_dict(obj.get("Meta"))
        orders = OrdersClass.from_dict(obj.get("Orders"))
        return Definitions(datum, iceberg, meta, orders)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Datum"] = to_class(Datum, self.datum)
        result["Iceberg"] = to_class(Iceberg, self.iceberg)
        result["Meta"] = to_class(MetaClass, self.meta)
        result["Orders"] = to_class(OrdersClass, self.orders)
        return result


class Orders:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'Orders':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return Orders(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def orders_from_dict(s: Any) -> Orders:
    return Orders.from_dict(s)


def orders_to_dict(x: Orders) -> Any:
    return to_class(Orders, x)
