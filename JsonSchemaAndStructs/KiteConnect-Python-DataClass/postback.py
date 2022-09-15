# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = postback_from_dict(json.loads(json_string))

from typing import Any, List, TypeVar, Type, cast, Callable
from enum import Enum


T = TypeVar("T")
EnumT = TypeVar("EnumT", bound=Enum)


def from_bool(x: Any) -> bool:
    assert isinstance(x, bool)
    return x


def from_str(x: Any) -> str:
    assert isinstance(x, str)
    return x


def to_enum(c: Type[EnumT], x: Any) -> EnumT:
    assert isinstance(x, c)
    return x.value


def to_class(c: Type[T], x: Any) -> dict:
    assert isinstance(x, c)
    return cast(Any, x).to_dict()


def from_list(f: Callable[[Any], T], x: Any) -> List[T]:
    assert isinstance(x, list)
    return [f(y) for y in x]


class Meta:
    additional_properties: bool
    title: str
    type: str

    def __init__(self, additional_properties: bool, title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Meta':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Meta(additional_properties, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class TypeEnum(Enum):
    INTEGER = "integer"
    NULL = "null"
    STRING = "string"


class AppID:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'AppID':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return AppID(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class Timestamp:
    format: str
    type: TypeEnum

    def __init__(self, format: str, type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Timestamp':
        assert isinstance(obj, dict)
        format = from_str(obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return Timestamp(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_str(self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class MetaClass:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'MetaClass':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return MetaClass(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class Properties:
    app_id: AppID
    average_price: AppID
    cancelled_quantity: AppID
    checksum: AppID
    disclosed_quantity: AppID
    exchange: AppID
    exchange_order_id: AppID
    exchange_timestamp: Timestamp
    exchange_update_timestamp: Timestamp
    filled_quantity: AppID
    guid: AppID
    instrument_token: AppID
    market_protection: AppID
    meta: MetaClass
    order_id: AppID
    order_timestamp: Timestamp
    order_type: AppID
    parent_order_id: AppID
    pending_quantity: AppID
    placed_by: AppID
    price: AppID
    product: AppID
    quantity: AppID
    status: AppID
    status_message: AppID
    status_message_raw: AppID
    tag: AppID
    tradingsymbol: AppID
    transaction_type: AppID
    trigger_price: AppID
    unfilled_quantity: AppID
    user_id: AppID
    validity: AppID
    variety: AppID

    def __init__(self, app_id: AppID, average_price: AppID, cancelled_quantity: AppID, checksum: AppID, disclosed_quantity: AppID, exchange: AppID, exchange_order_id: AppID, exchange_timestamp: Timestamp, exchange_update_timestamp: Timestamp, filled_quantity: AppID, guid: AppID, instrument_token: AppID, market_protection: AppID, meta: MetaClass, order_id: AppID, order_timestamp: Timestamp, order_type: AppID, parent_order_id: AppID, pending_quantity: AppID, placed_by: AppID, price: AppID, product: AppID, quantity: AppID, status: AppID, status_message: AppID, status_message_raw: AppID, tag: AppID, tradingsymbol: AppID, transaction_type: AppID, trigger_price: AppID, unfilled_quantity: AppID, user_id: AppID, validity: AppID, variety: AppID) -> None:
        self.app_id = app_id
        self.average_price = average_price
        self.cancelled_quantity = cancelled_quantity
        self.checksum = checksum
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
        self.tradingsymbol = tradingsymbol
        self.transaction_type = transaction_type
        self.trigger_price = trigger_price
        self.unfilled_quantity = unfilled_quantity
        self.user_id = user_id
        self.validity = validity
        self.variety = variety

    @staticmethod
    def from_dict(obj: Any) -> 'Properties':
        assert isinstance(obj, dict)
        app_id = AppID.from_dict(obj.get("app_id"))
        average_price = AppID.from_dict(obj.get("average_price"))
        cancelled_quantity = AppID.from_dict(obj.get("cancelled_quantity"))
        checksum = AppID.from_dict(obj.get("checksum"))
        disclosed_quantity = AppID.from_dict(obj.get("disclosed_quantity"))
        exchange = AppID.from_dict(obj.get("exchange"))
        exchange_order_id = AppID.from_dict(obj.get("exchange_order_id"))
        exchange_timestamp = Timestamp.from_dict(obj.get("exchange_timestamp"))
        exchange_update_timestamp = Timestamp.from_dict(obj.get("exchange_update_timestamp"))
        filled_quantity = AppID.from_dict(obj.get("filled_quantity"))
        guid = AppID.from_dict(obj.get("guid"))
        instrument_token = AppID.from_dict(obj.get("instrument_token"))
        market_protection = AppID.from_dict(obj.get("market_protection"))
        meta = MetaClass.from_dict(obj.get("meta"))
        order_id = AppID.from_dict(obj.get("order_id"))
        order_timestamp = Timestamp.from_dict(obj.get("order_timestamp"))
        order_type = AppID.from_dict(obj.get("order_type"))
        parent_order_id = AppID.from_dict(obj.get("parent_order_id"))
        pending_quantity = AppID.from_dict(obj.get("pending_quantity"))
        placed_by = AppID.from_dict(obj.get("placed_by"))
        price = AppID.from_dict(obj.get("price"))
        product = AppID.from_dict(obj.get("product"))
        quantity = AppID.from_dict(obj.get("quantity"))
        status = AppID.from_dict(obj.get("status"))
        status_message = AppID.from_dict(obj.get("status_message"))
        status_message_raw = AppID.from_dict(obj.get("status_message_raw"))
        tag = AppID.from_dict(obj.get("tag"))
        tradingsymbol = AppID.from_dict(obj.get("tradingsymbol"))
        transaction_type = AppID.from_dict(obj.get("transaction_type"))
        trigger_price = AppID.from_dict(obj.get("trigger_price"))
        unfilled_quantity = AppID.from_dict(obj.get("unfilled_quantity"))
        user_id = AppID.from_dict(obj.get("user_id"))
        validity = AppID.from_dict(obj.get("validity"))
        variety = AppID.from_dict(obj.get("variety"))
        return Properties(app_id, average_price, cancelled_quantity, checksum, disclosed_quantity, exchange, exchange_order_id, exchange_timestamp, exchange_update_timestamp, filled_quantity, guid, instrument_token, market_protection, meta, order_id, order_timestamp, order_type, parent_order_id, pending_quantity, placed_by, price, product, quantity, status, status_message, status_message_raw, tag, tradingsymbol, transaction_type, trigger_price, unfilled_quantity, user_id, validity, variety)

    def to_dict(self) -> dict:
        result: dict = {}
        result["app_id"] = to_class(AppID, self.app_id)
        result["average_price"] = to_class(AppID, self.average_price)
        result["cancelled_quantity"] = to_class(AppID, self.cancelled_quantity)
        result["checksum"] = to_class(AppID, self.checksum)
        result["disclosed_quantity"] = to_class(AppID, self.disclosed_quantity)
        result["exchange"] = to_class(AppID, self.exchange)
        result["exchange_order_id"] = to_class(AppID, self.exchange_order_id)
        result["exchange_timestamp"] = to_class(Timestamp, self.exchange_timestamp)
        result["exchange_update_timestamp"] = to_class(Timestamp, self.exchange_update_timestamp)
        result["filled_quantity"] = to_class(AppID, self.filled_quantity)
        result["guid"] = to_class(AppID, self.guid)
        result["instrument_token"] = to_class(AppID, self.instrument_token)
        result["market_protection"] = to_class(AppID, self.market_protection)
        result["meta"] = to_class(MetaClass, self.meta)
        result["order_id"] = to_class(AppID, self.order_id)
        result["order_timestamp"] = to_class(Timestamp, self.order_timestamp)
        result["order_type"] = to_class(AppID, self.order_type)
        result["parent_order_id"] = to_class(AppID, self.parent_order_id)
        result["pending_quantity"] = to_class(AppID, self.pending_quantity)
        result["placed_by"] = to_class(AppID, self.placed_by)
        result["price"] = to_class(AppID, self.price)
        result["product"] = to_class(AppID, self.product)
        result["quantity"] = to_class(AppID, self.quantity)
        result["status"] = to_class(AppID, self.status)
        result["status_message"] = to_class(AppID, self.status_message)
        result["status_message_raw"] = to_class(AppID, self.status_message_raw)
        result["tag"] = to_class(AppID, self.tag)
        result["tradingsymbol"] = to_class(AppID, self.tradingsymbol)
        result["transaction_type"] = to_class(AppID, self.transaction_type)
        result["trigger_price"] = to_class(AppID, self.trigger_price)
        result["unfilled_quantity"] = to_class(AppID, self.unfilled_quantity)
        result["user_id"] = to_class(AppID, self.user_id)
        result["validity"] = to_class(AppID, self.validity)
        result["variety"] = to_class(AppID, self.variety)
        return result


class PostbackClass:
    additional_properties: bool
    properties: Properties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: Properties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'PostbackClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = Properties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return PostbackClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(Properties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    meta: Meta
    postback: PostbackClass

    def __init__(self, meta: Meta, postback: PostbackClass) -> None:
        self.meta = meta
        self.postback = postback

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        meta = Meta.from_dict(obj.get("Meta"))
        postback = PostbackClass.from_dict(obj.get("Postback"))
        return Definitions(meta, postback)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Meta"] = to_class(Meta, self.meta)
        result["Postback"] = to_class(PostbackClass, self.postback)
        return result


class Postback:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'Postback':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return Postback(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def postback_from_dict(s: Any) -> Postback:
    return Postback.from_dict(s)


def postback_to_dict(x: Postback) -> Any:
    return to_class(Postback, x)
