# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = gtt_get_order_from_dict(json.loads(json_string))

from enum import Enum
from typing import Any, List, Optional, TypeVar, Type, cast, Callable


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


class TypeEnum(Enum):
    INTEGER = "integer"
    NULL = "null"
    NUMBER = "number"
    STRING = "string"


class Exchange:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Exchange':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return Exchange(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class TriggerValues:
    items: Exchange
    type: str

    def __init__(self, items: Exchange, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'TriggerValues':
        assert isinstance(obj, dict)
        items = Exchange.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return TriggerValues(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(Exchange, self.items)
        result["type"] = from_str(self.type)
        return result


class ConditionProperties:
    exchange: Exchange
    instrument_token: Exchange
    last_price: Exchange
    tradingsymbol: Exchange
    trigger_values: TriggerValues

    def __init__(self, exchange: Exchange, instrument_token: Exchange, last_price: Exchange, tradingsymbol: Exchange, trigger_values: TriggerValues) -> None:
        self.exchange = exchange
        self.instrument_token = instrument_token
        self.last_price = last_price
        self.tradingsymbol = tradingsymbol
        self.trigger_values = trigger_values

    @staticmethod
    def from_dict(obj: Any) -> 'ConditionProperties':
        assert isinstance(obj, dict)
        exchange = Exchange.from_dict(obj.get("exchange"))
        instrument_token = Exchange.from_dict(obj.get("instrument_token"))
        last_price = Exchange.from_dict(obj.get("last_price"))
        tradingsymbol = Exchange.from_dict(obj.get("tradingsymbol"))
        trigger_values = TriggerValues.from_dict(obj.get("trigger_values"))
        return ConditionProperties(exchange, instrument_token, last_price, tradingsymbol, trigger_values)

    def to_dict(self) -> dict:
        result: dict = {}
        result["exchange"] = to_class(Exchange, self.exchange)
        result["instrument_token"] = to_class(Exchange, self.instrument_token)
        result["last_price"] = to_class(Exchange, self.last_price)
        result["tradingsymbol"] = to_class(Exchange, self.tradingsymbol)
        result["trigger_values"] = to_class(TriggerValues, self.trigger_values)
        return result


class Condition:
    additional_properties: bool
    properties: ConditionProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: ConditionProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Condition':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = ConditionProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Condition(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(ConditionProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class ConditionClass:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'ConditionClass':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return ConditionClass(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class CreatedAt:
    format: str
    type: TypeEnum

    def __init__(self, format: str, type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'CreatedAt':
        assert isinstance(obj, dict)
        format = from_str(obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return CreatedAt(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_str(self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class Orders:
    items: ConditionClass
    type: str

    def __init__(self, items: ConditionClass, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Orders':
        assert isinstance(obj, dict)
        items = ConditionClass.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return Orders(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(ConditionClass, self.items)
        result["type"] = from_str(self.type)
        return result


class DataProperties:
    condition: ConditionClass
    created_at: CreatedAt
    expires_at: CreatedAt
    id: Exchange
    meta: Exchange
    orders: Orders
    parent_trigger: Exchange
    status: Exchange
    type: Exchange
    updated_at: CreatedAt
    user_id: Exchange

    def __init__(self, condition: ConditionClass, created_at: CreatedAt, expires_at: CreatedAt, id: Exchange, meta: Exchange, orders: Orders, parent_trigger: Exchange, status: Exchange, type: Exchange, updated_at: CreatedAt, user_id: Exchange) -> None:
        self.condition = condition
        self.created_at = created_at
        self.expires_at = expires_at
        self.id = id
        self.meta = meta
        self.orders = orders
        self.parent_trigger = parent_trigger
        self.status = status
        self.type = type
        self.updated_at = updated_at
        self.user_id = user_id

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        condition = ConditionClass.from_dict(obj.get("condition"))
        created_at = CreatedAt.from_dict(obj.get("created_at"))
        expires_at = CreatedAt.from_dict(obj.get("expires_at"))
        id = Exchange.from_dict(obj.get("id"))
        meta = Exchange.from_dict(obj.get("meta"))
        orders = Orders.from_dict(obj.get("orders"))
        parent_trigger = Exchange.from_dict(obj.get("parent_trigger"))
        status = Exchange.from_dict(obj.get("status"))
        type = Exchange.from_dict(obj.get("type"))
        updated_at = CreatedAt.from_dict(obj.get("updated_at"))
        user_id = Exchange.from_dict(obj.get("user_id"))
        return DataProperties(condition, created_at, expires_at, id, meta, orders, parent_trigger, status, type, updated_at, user_id)

    def to_dict(self) -> dict:
        result: dict = {}
        result["condition"] = to_class(ConditionClass, self.condition)
        result["created_at"] = to_class(CreatedAt, self.created_at)
        result["expires_at"] = to_class(CreatedAt, self.expires_at)
        result["id"] = to_class(Exchange, self.id)
        result["meta"] = to_class(Exchange, self.meta)
        result["orders"] = to_class(Orders, self.orders)
        result["parent_trigger"] = to_class(Exchange, self.parent_trigger)
        result["status"] = to_class(Exchange, self.status)
        result["type"] = to_class(Exchange, self.type)
        result["updated_at"] = to_class(CreatedAt, self.updated_at)
        result["user_id"] = to_class(Exchange, self.user_id)
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


class GttGetOrderProperties:
    data: ConditionClass
    status: Exchange

    def __init__(self, data: ConditionClass, status: Exchange) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'GttGetOrderProperties':
        assert isinstance(obj, dict)
        data = ConditionClass.from_dict(obj.get("data"))
        status = Exchange.from_dict(obj.get("status"))
        return GttGetOrderProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(ConditionClass, self.data)
        result["status"] = to_class(Exchange, self.status)
        return result


class GttGetOrderClass:
    additional_properties: bool
    properties: GttGetOrderProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: GttGetOrderProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'GttGetOrderClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = GttGetOrderProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return GttGetOrderClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(GttGetOrderProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class AnyOf:
    ref: Optional[str]
    type: Optional[TypeEnum]

    def __init__(self, ref: Optional[str], type: Optional[TypeEnum]) -> None:
        self.ref = ref
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'AnyOf':
        assert isinstance(obj, dict)
        ref = from_union([from_str, from_none], obj.get("$ref"))
        type = from_union([TypeEnum, from_none], obj.get("type"))
        return AnyOf(ref, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_union([from_str, from_none], self.ref)
        result["type"] = from_union([lambda x: to_enum(TypeEnum, x), from_none], self.type)
        return result


class Result:
    any_of: List[AnyOf]

    def __init__(self, any_of: List[AnyOf]) -> None:
        self.any_of = any_of

    @staticmethod
    def from_dict(obj: Any) -> 'Result':
        assert isinstance(obj, dict)
        any_of = from_list(AnyOf.from_dict, obj.get("anyOf"))
        return Result(any_of)

    def to_dict(self) -> dict:
        result: dict = {}
        result["anyOf"] = from_list(lambda x: to_class(AnyOf, x), self.any_of)
        return result


class OrderProperties:
    exchange: Exchange
    order_type: Exchange
    price: Exchange
    product: Exchange
    quantity: Exchange
    result: Result
    tradingsymbol: Exchange
    transaction_type: Exchange

    def __init__(self, exchange: Exchange, order_type: Exchange, price: Exchange, product: Exchange, quantity: Exchange, result: Result, tradingsymbol: Exchange, transaction_type: Exchange) -> None:
        self.exchange = exchange
        self.order_type = order_type
        self.price = price
        self.product = product
        self.quantity = quantity
        self.result = result
        self.tradingsymbol = tradingsymbol
        self.transaction_type = transaction_type

    @staticmethod
    def from_dict(obj: Any) -> 'OrderProperties':
        assert isinstance(obj, dict)
        exchange = Exchange.from_dict(obj.get("exchange"))
        order_type = Exchange.from_dict(obj.get("order_type"))
        price = Exchange.from_dict(obj.get("price"))
        product = Exchange.from_dict(obj.get("product"))
        quantity = Exchange.from_dict(obj.get("quantity"))
        result = Result.from_dict(obj.get("result"))
        tradingsymbol = Exchange.from_dict(obj.get("tradingsymbol"))
        transaction_type = Exchange.from_dict(obj.get("transaction_type"))
        return OrderProperties(exchange, order_type, price, product, quantity, result, tradingsymbol, transaction_type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["exchange"] = to_class(Exchange, self.exchange)
        result["order_type"] = to_class(Exchange, self.order_type)
        result["price"] = to_class(Exchange, self.price)
        result["product"] = to_class(Exchange, self.product)
        result["quantity"] = to_class(Exchange, self.quantity)
        result["result"] = to_class(Result, self.result)
        result["tradingsymbol"] = to_class(Exchange, self.tradingsymbol)
        result["transaction_type"] = to_class(Exchange, self.transaction_type)
        return result


class Order:
    additional_properties: bool
    properties: OrderProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OrderProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Order':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OrderProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Order(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OrderProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class OrderResultProperties:
    order_id: Exchange
    rejection_reason: Exchange
    status: Exchange

    def __init__(self, order_id: Exchange, rejection_reason: Exchange, status: Exchange) -> None:
        self.order_id = order_id
        self.rejection_reason = rejection_reason
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'OrderResultProperties':
        assert isinstance(obj, dict)
        order_id = Exchange.from_dict(obj.get("order_id"))
        rejection_reason = Exchange.from_dict(obj.get("rejection_reason"))
        status = Exchange.from_dict(obj.get("status"))
        return OrderResultProperties(order_id, rejection_reason, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["order_id"] = to_class(Exchange, self.order_id)
        result["rejection_reason"] = to_class(Exchange, self.rejection_reason)
        result["status"] = to_class(Exchange, self.status)
        return result


class OrderResult:
    additional_properties: bool
    properties: OrderResultProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OrderResultProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'OrderResult':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OrderResultProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return OrderResult(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OrderResultProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class ResultProperties:
    account_id: Exchange
    exchange: Exchange
    meta: Exchange
    order_result: ConditionClass
    order_type: Exchange
    price: Exchange
    product: Exchange
    quantity: Exchange
    timestamp: CreatedAt
    tradingsymbol: Exchange
    transaction_type: Exchange
    triggered_at: Exchange
    validity: Exchange

    def __init__(self, account_id: Exchange, exchange: Exchange, meta: Exchange, order_result: ConditionClass, order_type: Exchange, price: Exchange, product: Exchange, quantity: Exchange, timestamp: CreatedAt, tradingsymbol: Exchange, transaction_type: Exchange, triggered_at: Exchange, validity: Exchange) -> None:
        self.account_id = account_id
        self.exchange = exchange
        self.meta = meta
        self.order_result = order_result
        self.order_type = order_type
        self.price = price
        self.product = product
        self.quantity = quantity
        self.timestamp = timestamp
        self.tradingsymbol = tradingsymbol
        self.transaction_type = transaction_type
        self.triggered_at = triggered_at
        self.validity = validity

    @staticmethod
    def from_dict(obj: Any) -> 'ResultProperties':
        assert isinstance(obj, dict)
        account_id = Exchange.from_dict(obj.get("account_id"))
        exchange = Exchange.from_dict(obj.get("exchange"))
        meta = Exchange.from_dict(obj.get("meta"))
        order_result = ConditionClass.from_dict(obj.get("order_result"))
        order_type = Exchange.from_dict(obj.get("order_type"))
        price = Exchange.from_dict(obj.get("price"))
        product = Exchange.from_dict(obj.get("product"))
        quantity = Exchange.from_dict(obj.get("quantity"))
        timestamp = CreatedAt.from_dict(obj.get("timestamp"))
        tradingsymbol = Exchange.from_dict(obj.get("tradingsymbol"))
        transaction_type = Exchange.from_dict(obj.get("transaction_type"))
        triggered_at = Exchange.from_dict(obj.get("triggered_at"))
        validity = Exchange.from_dict(obj.get("validity"))
        return ResultProperties(account_id, exchange, meta, order_result, order_type, price, product, quantity, timestamp, tradingsymbol, transaction_type, triggered_at, validity)

    def to_dict(self) -> dict:
        result: dict = {}
        result["account_id"] = to_class(Exchange, self.account_id)
        result["exchange"] = to_class(Exchange, self.exchange)
        result["meta"] = to_class(Exchange, self.meta)
        result["order_result"] = to_class(ConditionClass, self.order_result)
        result["order_type"] = to_class(Exchange, self.order_type)
        result["price"] = to_class(Exchange, self.price)
        result["product"] = to_class(Exchange, self.product)
        result["quantity"] = to_class(Exchange, self.quantity)
        result["timestamp"] = to_class(CreatedAt, self.timestamp)
        result["tradingsymbol"] = to_class(Exchange, self.tradingsymbol)
        result["transaction_type"] = to_class(Exchange, self.transaction_type)
        result["triggered_at"] = to_class(Exchange, self.triggered_at)
        result["validity"] = to_class(Exchange, self.validity)
        return result


class ResultClass:
    additional_properties: bool
    properties: ResultProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: ResultProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'ResultClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = ResultProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return ResultClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(ResultProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    condition: Condition
    data: Data
    gtt_get_order: GttGetOrderClass
    order: Order
    order_result: OrderResult
    result: ResultClass

    def __init__(self, condition: Condition, data: Data, gtt_get_order: GttGetOrderClass, order: Order, order_result: OrderResult, result: ResultClass) -> None:
        self.condition = condition
        self.data = data
        self.gtt_get_order = gtt_get_order
        self.order = order
        self.order_result = order_result
        self.result = result

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        condition = Condition.from_dict(obj.get("Condition"))
        data = Data.from_dict(obj.get("Data"))
        gtt_get_order = GttGetOrderClass.from_dict(obj.get("GttGetOrder"))
        order = Order.from_dict(obj.get("Order"))
        order_result = OrderResult.from_dict(obj.get("OrderResult"))
        result = ResultClass.from_dict(obj.get("Result"))
        return Definitions(condition, data, gtt_get_order, order, order_result, result)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Condition"] = to_class(Condition, self.condition)
        result["Data"] = to_class(Data, self.data)
        result["GttGetOrder"] = to_class(GttGetOrderClass, self.gtt_get_order)
        result["Order"] = to_class(Order, self.order)
        result["OrderResult"] = to_class(OrderResult, self.order_result)
        result["Result"] = to_class(ResultClass, self.result)
        return result


class GttGetOrder:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'GttGetOrder':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return GttGetOrder(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def gtt_get_order_from_dict(s: Any) -> GttGetOrder:
    return GttGetOrder.from_dict(s)


def gtt_get_order_to_dict(x: GttGetOrder) -> Any:
    return to_class(GttGetOrder, x)
