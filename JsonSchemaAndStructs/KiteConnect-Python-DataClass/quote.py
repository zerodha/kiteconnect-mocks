# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = quote_from_dict(json.loads(json_string))

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


class Orders:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Orders':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return Orders(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class BuyProperties:
    orders: Orders
    price: Orders
    quantity: Orders

    def __init__(self, orders: Orders, price: Orders, quantity: Orders) -> None:
        self.orders = orders
        self.price = price
        self.quantity = quantity

    @staticmethod
    def from_dict(obj: Any) -> 'BuyProperties':
        assert isinstance(obj, dict)
        orders = Orders.from_dict(obj.get("orders"))
        price = Orders.from_dict(obj.get("price"))
        quantity = Orders.from_dict(obj.get("quantity"))
        return BuyProperties(orders, price, quantity)

    def to_dict(self) -> dict:
        result: dict = {}
        result["orders"] = to_class(Orders, self.orders)
        result["price"] = to_class(Orders, self.price)
        result["quantity"] = to_class(Orders, self.quantity)
        return result


class Buy:
    additional_properties: bool
    properties: BuyProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: BuyProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Buy':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = BuyProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Buy(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(BuyProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class NseInfy:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'NseInfy':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return NseInfy(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class DataProperties:
    nse_infy: NseInfy

    def __init__(self, nse_infy: NseInfy) -> None:
        self.nse_infy = nse_infy

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        nse_infy = NseInfy.from_dict(obj.get("NSE:INFY"))
        return DataProperties(nse_infy)

    def to_dict(self) -> dict:
        result: dict = {}
        result["NSE:INFY"] = to_class(NseInfy, self.nse_infy)
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


class BuyClass:
    items: NseInfy
    type: str

    def __init__(self, items: NseInfy, type: str) -> None:
        self.items = items
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'BuyClass':
        assert isinstance(obj, dict)
        items = NseInfy.from_dict(obj.get("items"))
        type = from_str(obj.get("type"))
        return BuyClass(items, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["items"] = to_class(NseInfy, self.items)
        result["type"] = from_str(self.type)
        return result


class DepthProperties:
    buy: BuyClass
    sell: BuyClass

    def __init__(self, buy: BuyClass, sell: BuyClass) -> None:
        self.buy = buy
        self.sell = sell

    @staticmethod
    def from_dict(obj: Any) -> 'DepthProperties':
        assert isinstance(obj, dict)
        buy = BuyClass.from_dict(obj.get("buy"))
        sell = BuyClass.from_dict(obj.get("sell"))
        return DepthProperties(buy, sell)

    def to_dict(self) -> dict:
        result: dict = {}
        result["buy"] = to_class(BuyClass, self.buy)
        result["sell"] = to_class(BuyClass, self.sell)
        return result


class Depth:
    additional_properties: bool
    properties: DepthProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: DepthProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Depth':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = DepthProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Depth(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(DepthProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class LastTradeTime:
    format: str
    type: TypeEnum

    def __init__(self, format: str, type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'LastTradeTime':
        assert isinstance(obj, dict)
        format = from_str(obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return LastTradeTime(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_str(self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class NseInfyProperties:
    average_price: Orders
    buy_quantity: Orders
    depth: NseInfy
    instrument_token: Orders
    last_price: Orders
    last_quantity: Orders
    last_trade_time: LastTradeTime
    lower_circuit_limit: Orders
    net_change: Orders
    ohlc: NseInfy
    oi: Orders
    oi_day_high: Orders
    oi_day_low: Orders
    sell_quantity: Orders
    timestamp: LastTradeTime
    upper_circuit_limit: Orders
    volume: Orders

    def __init__(self, average_price: Orders, buy_quantity: Orders, depth: NseInfy, instrument_token: Orders, last_price: Orders, last_quantity: Orders, last_trade_time: LastTradeTime, lower_circuit_limit: Orders, net_change: Orders, ohlc: NseInfy, oi: Orders, oi_day_high: Orders, oi_day_low: Orders, sell_quantity: Orders, timestamp: LastTradeTime, upper_circuit_limit: Orders, volume: Orders) -> None:
        self.average_price = average_price
        self.buy_quantity = buy_quantity
        self.depth = depth
        self.instrument_token = instrument_token
        self.last_price = last_price
        self.last_quantity = last_quantity
        self.last_trade_time = last_trade_time
        self.lower_circuit_limit = lower_circuit_limit
        self.net_change = net_change
        self.ohlc = ohlc
        self.oi = oi
        self.oi_day_high = oi_day_high
        self.oi_day_low = oi_day_low
        self.sell_quantity = sell_quantity
        self.timestamp = timestamp
        self.upper_circuit_limit = upper_circuit_limit
        self.volume = volume

    @staticmethod
    def from_dict(obj: Any) -> 'NseInfyProperties':
        assert isinstance(obj, dict)
        average_price = Orders.from_dict(obj.get("average_price"))
        buy_quantity = Orders.from_dict(obj.get("buy_quantity"))
        depth = NseInfy.from_dict(obj.get("depth"))
        instrument_token = Orders.from_dict(obj.get("instrument_token"))
        last_price = Orders.from_dict(obj.get("last_price"))
        last_quantity = Orders.from_dict(obj.get("last_quantity"))
        last_trade_time = LastTradeTime.from_dict(obj.get("last_trade_time"))
        lower_circuit_limit = Orders.from_dict(obj.get("lower_circuit_limit"))
        net_change = Orders.from_dict(obj.get("net_change"))
        ohlc = NseInfy.from_dict(obj.get("ohlc"))
        oi = Orders.from_dict(obj.get("oi"))
        oi_day_high = Orders.from_dict(obj.get("oi_day_high"))
        oi_day_low = Orders.from_dict(obj.get("oi_day_low"))
        sell_quantity = Orders.from_dict(obj.get("sell_quantity"))
        timestamp = LastTradeTime.from_dict(obj.get("timestamp"))
        upper_circuit_limit = Orders.from_dict(obj.get("upper_circuit_limit"))
        volume = Orders.from_dict(obj.get("volume"))
        return NseInfyProperties(average_price, buy_quantity, depth, instrument_token, last_price, last_quantity, last_trade_time, lower_circuit_limit, net_change, ohlc, oi, oi_day_high, oi_day_low, sell_quantity, timestamp, upper_circuit_limit, volume)

    def to_dict(self) -> dict:
        result: dict = {}
        result["average_price"] = to_class(Orders, self.average_price)
        result["buy_quantity"] = to_class(Orders, self.buy_quantity)
        result["depth"] = to_class(NseInfy, self.depth)
        result["instrument_token"] = to_class(Orders, self.instrument_token)
        result["last_price"] = to_class(Orders, self.last_price)
        result["last_quantity"] = to_class(Orders, self.last_quantity)
        result["last_trade_time"] = to_class(LastTradeTime, self.last_trade_time)
        result["lower_circuit_limit"] = to_class(Orders, self.lower_circuit_limit)
        result["net_change"] = to_class(Orders, self.net_change)
        result["ohlc"] = to_class(NseInfy, self.ohlc)
        result["oi"] = to_class(Orders, self.oi)
        result["oi_day_high"] = to_class(Orders, self.oi_day_high)
        result["oi_day_low"] = to_class(Orders, self.oi_day_low)
        result["sell_quantity"] = to_class(Orders, self.sell_quantity)
        result["timestamp"] = to_class(LastTradeTime, self.timestamp)
        result["upper_circuit_limit"] = to_class(Orders, self.upper_circuit_limit)
        result["volume"] = to_class(Orders, self.volume)
        return result


class NseInfyClass:
    additional_properties: bool
    properties: NseInfyProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: NseInfyProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'NseInfyClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = NseInfyProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return NseInfyClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(NseInfyProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class OhlcProperties:
    close: Orders
    high: Orders
    low: Orders
    open: Orders

    def __init__(self, close: Orders, high: Orders, low: Orders, open: Orders) -> None:
        self.close = close
        self.high = high
        self.low = low
        self.open = open

    @staticmethod
    def from_dict(obj: Any) -> 'OhlcProperties':
        assert isinstance(obj, dict)
        close = Orders.from_dict(obj.get("close"))
        high = Orders.from_dict(obj.get("high"))
        low = Orders.from_dict(obj.get("low"))
        open = Orders.from_dict(obj.get("open"))
        return OhlcProperties(close, high, low, open)

    def to_dict(self) -> dict:
        result: dict = {}
        result["close"] = to_class(Orders, self.close)
        result["high"] = to_class(Orders, self.high)
        result["low"] = to_class(Orders, self.low)
        result["open"] = to_class(Orders, self.open)
        return result


class Ohlc:
    additional_properties: bool
    properties: OhlcProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: OhlcProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Ohlc':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = OhlcProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Ohlc(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(OhlcProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class QuoteProperties:
    data: NseInfy
    status: Orders

    def __init__(self, data: NseInfy, status: Orders) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'QuoteProperties':
        assert isinstance(obj, dict)
        data = NseInfy.from_dict(obj.get("data"))
        status = Orders.from_dict(obj.get("status"))
        return QuoteProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(NseInfy, self.data)
        result["status"] = to_class(Orders, self.status)
        return result


class QuoteClass:
    additional_properties: bool
    properties: QuoteProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: QuoteProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'QuoteClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = QuoteProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return QuoteClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(QuoteProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    buy: Buy
    data: Data
    depth: Depth
    nse_infy: NseInfyClass
    ohlc: Ohlc
    quote: QuoteClass

    def __init__(self, buy: Buy, data: Data, depth: Depth, nse_infy: NseInfyClass, ohlc: Ohlc, quote: QuoteClass) -> None:
        self.buy = buy
        self.data = data
        self.depth = depth
        self.nse_infy = nse_infy
        self.ohlc = ohlc
        self.quote = quote

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        buy = Buy.from_dict(obj.get("Buy"))
        data = Data.from_dict(obj.get("Data"))
        depth = Depth.from_dict(obj.get("Depth"))
        nse_infy = NseInfyClass.from_dict(obj.get("NseInfy"))
        ohlc = Ohlc.from_dict(obj.get("Ohlc"))
        quote = QuoteClass.from_dict(obj.get("Quote"))
        return Definitions(buy, data, depth, nse_infy, ohlc, quote)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Buy"] = to_class(Buy, self.buy)
        result["Data"] = to_class(Data, self.data)
        result["Depth"] = to_class(Depth, self.depth)
        result["NseInfy"] = to_class(NseInfyClass, self.nse_infy)
        result["Ohlc"] = to_class(Ohlc, self.ohlc)
        result["Quote"] = to_class(QuoteClass, self.quote)
        return result


class Quote:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'Quote':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return Quote(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def quote_from_dict(s: Any) -> Quote:
    return Quote.from_dict(s)


def quote_to_dict(x: Quote) -> Any:
    return to_class(Quote, x)
