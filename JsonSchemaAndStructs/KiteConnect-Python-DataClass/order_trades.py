# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = order_trades_from_dict(json.loads(json_string))

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


class ExchangeTimestamp:
    format: str
    type: TypeEnum

    def __init__(self, format: str, type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'ExchangeTimestamp':
        assert isinstance(obj, dict)
        format = from_str(obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return ExchangeTimestamp(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_str(self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class DatumProperties:
    average_price: AveragePrice
    exchange: AveragePrice
    exchange_order_id: AveragePrice
    exchange_timestamp: ExchangeTimestamp
    fill_timestamp: ExchangeTimestamp
    instrument_token: AveragePrice
    order_id: AveragePrice
    order_timestamp: ExchangeTimestamp
    product: AveragePrice
    quantity: AveragePrice
    trade_id: ExchangeTimestamp
    tradingsymbol: AveragePrice
    transaction_type: AveragePrice

    def __init__(self, average_price: AveragePrice, exchange: AveragePrice, exchange_order_id: AveragePrice, exchange_timestamp: ExchangeTimestamp, fill_timestamp: ExchangeTimestamp, instrument_token: AveragePrice, order_id: AveragePrice, order_timestamp: ExchangeTimestamp, product: AveragePrice, quantity: AveragePrice, trade_id: ExchangeTimestamp, tradingsymbol: AveragePrice, transaction_type: AveragePrice) -> None:
        self.average_price = average_price
        self.exchange = exchange
        self.exchange_order_id = exchange_order_id
        self.exchange_timestamp = exchange_timestamp
        self.fill_timestamp = fill_timestamp
        self.instrument_token = instrument_token
        self.order_id = order_id
        self.order_timestamp = order_timestamp
        self.product = product
        self.quantity = quantity
        self.trade_id = trade_id
        self.tradingsymbol = tradingsymbol
        self.transaction_type = transaction_type

    @staticmethod
    def from_dict(obj: Any) -> 'DatumProperties':
        assert isinstance(obj, dict)
        average_price = AveragePrice.from_dict(obj.get("average_price"))
        exchange = AveragePrice.from_dict(obj.get("exchange"))
        exchange_order_id = AveragePrice.from_dict(obj.get("exchange_order_id"))
        exchange_timestamp = ExchangeTimestamp.from_dict(obj.get("exchange_timestamp"))
        fill_timestamp = ExchangeTimestamp.from_dict(obj.get("fill_timestamp"))
        instrument_token = AveragePrice.from_dict(obj.get("instrument_token"))
        order_id = AveragePrice.from_dict(obj.get("order_id"))
        order_timestamp = ExchangeTimestamp.from_dict(obj.get("order_timestamp"))
        product = AveragePrice.from_dict(obj.get("product"))
        quantity = AveragePrice.from_dict(obj.get("quantity"))
        trade_id = ExchangeTimestamp.from_dict(obj.get("trade_id"))
        tradingsymbol = AveragePrice.from_dict(obj.get("tradingsymbol"))
        transaction_type = AveragePrice.from_dict(obj.get("transaction_type"))
        return DatumProperties(average_price, exchange, exchange_order_id, exchange_timestamp, fill_timestamp, instrument_token, order_id, order_timestamp, product, quantity, trade_id, tradingsymbol, transaction_type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["average_price"] = to_class(AveragePrice, self.average_price)
        result["exchange"] = to_class(AveragePrice, self.exchange)
        result["exchange_order_id"] = to_class(AveragePrice, self.exchange_order_id)
        result["exchange_timestamp"] = to_class(ExchangeTimestamp, self.exchange_timestamp)
        result["fill_timestamp"] = to_class(ExchangeTimestamp, self.fill_timestamp)
        result["instrument_token"] = to_class(AveragePrice, self.instrument_token)
        result["order_id"] = to_class(AveragePrice, self.order_id)
        result["order_timestamp"] = to_class(ExchangeTimestamp, self.order_timestamp)
        result["product"] = to_class(AveragePrice, self.product)
        result["quantity"] = to_class(AveragePrice, self.quantity)
        result["trade_id"] = to_class(ExchangeTimestamp, self.trade_id)
        result["tradingsymbol"] = to_class(AveragePrice, self.tradingsymbol)
        result["transaction_type"] = to_class(AveragePrice, self.transaction_type)
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


class OrderTradesProperties:
    data: Data
    status: AveragePrice

    def __init__(self, data: Data, status: AveragePrice) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'OrderTradesProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        status = AveragePrice.from_dict(obj.get("status"))
        return OrderTradesProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        result["status"] = to_class(AveragePrice, self.status)
        return result


class OrderTradesClass:
    additional_properties: bool
    properties: OrderTradesProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OrderTradesProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'OrderTradesClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OrderTradesProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return OrderTradesClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OrderTradesProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    datum: Datum
    order_trades: OrderTradesClass

    def __init__(self, datum: Datum, order_trades: OrderTradesClass) -> None:
        self.datum = datum
        self.order_trades = order_trades

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        datum = Datum.from_dict(obj.get("Datum"))
        order_trades = OrderTradesClass.from_dict(obj.get("OrderTrades"))
        return Definitions(datum, order_trades)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Datum"] = to_class(Datum, self.datum)
        result["OrderTrades"] = to_class(OrderTradesClass, self.order_trades)
        return result


class OrderTrades:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'OrderTrades':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return OrderTrades(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def order_trades_from_dict(s: Any) -> OrderTrades:
    return OrderTrades.from_dict(s)


def order_trades_to_dict(x: OrderTrades) -> Any:
    return to_class(OrderTrades, x)
