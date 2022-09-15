# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = margin_commodity_from_dict(json.loads(json_string))

from typing import Any, List, TypeVar, Type, cast, Callable


T = TypeVar("T")


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


class AdhocMargin:
    type: str

    def __init__(self, type: str) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'AdhocMargin':
        assert isinstance(obj, dict)
        type = from_str(obj.get("type"))
        return AdhocMargin(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = from_str(self.type)
        return result


class AvailableProperties:
    adhoc_margin: AdhocMargin
    cash: AdhocMargin
    collateral: AdhocMargin
    intraday_payin: AdhocMargin
    live_balance: AdhocMargin
    opening_balance: AdhocMargin

    def __init__(self, adhoc_margin: AdhocMargin, cash: AdhocMargin, collateral: AdhocMargin, intraday_payin: AdhocMargin, live_balance: AdhocMargin, opening_balance: AdhocMargin) -> None:
        self.adhoc_margin = adhoc_margin
        self.cash = cash
        self.collateral = collateral
        self.intraday_payin = intraday_payin
        self.live_balance = live_balance
        self.opening_balance = opening_balance

    @staticmethod
    def from_dict(obj: Any) -> 'AvailableProperties':
        assert isinstance(obj, dict)
        adhoc_margin = AdhocMargin.from_dict(obj.get("adhoc_margin"))
        cash = AdhocMargin.from_dict(obj.get("cash"))
        collateral = AdhocMargin.from_dict(obj.get("collateral"))
        intraday_payin = AdhocMargin.from_dict(obj.get("intraday_payin"))
        live_balance = AdhocMargin.from_dict(obj.get("live_balance"))
        opening_balance = AdhocMargin.from_dict(obj.get("opening_balance"))
        return AvailableProperties(adhoc_margin, cash, collateral, intraday_payin, live_balance, opening_balance)

    def to_dict(self) -> dict:
        result: dict = {}
        result["adhoc_margin"] = to_class(AdhocMargin, self.adhoc_margin)
        result["cash"] = to_class(AdhocMargin, self.cash)
        result["collateral"] = to_class(AdhocMargin, self.collateral)
        result["intraday_payin"] = to_class(AdhocMargin, self.intraday_payin)
        result["live_balance"] = to_class(AdhocMargin, self.live_balance)
        result["opening_balance"] = to_class(AdhocMargin, self.opening_balance)
        return result


class Available:
    additional_properties: bool
    properties: AvailableProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: AvailableProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Available':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = AvailableProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return Available(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(AvailableProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class AvailableClass:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'AvailableClass':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return AvailableClass(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class Utilised:
    additional_properties: AdhocMargin
    type: str

    def __init__(self, additional_properties: AdhocMargin, type: str) -> None:
        self.additional_properties = additional_properties
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Utilised':
        assert isinstance(obj, dict)
        additional_properties = AdhocMargin.from_dict(obj.get("additionalProperties"))
        type = from_str(obj.get("type"))
        return Utilised(additional_properties, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = to_class(AdhocMargin, self.additional_properties)
        result["type"] = from_str(self.type)
        return result


class DataProperties:
    available: AvailableClass
    enabled: AdhocMargin
    net: AdhocMargin
    utilised: Utilised

    def __init__(self, available: AvailableClass, enabled: AdhocMargin, net: AdhocMargin, utilised: Utilised) -> None:
        self.available = available
        self.enabled = enabled
        self.net = net
        self.utilised = utilised

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        available = AvailableClass.from_dict(obj.get("available"))
        enabled = AdhocMargin.from_dict(obj.get("enabled"))
        net = AdhocMargin.from_dict(obj.get("net"))
        utilised = Utilised.from_dict(obj.get("utilised"))
        return DataProperties(available, enabled, net, utilised)

    def to_dict(self) -> dict:
        result: dict = {}
        result["available"] = to_class(AvailableClass, self.available)
        result["enabled"] = to_class(AdhocMargin, self.enabled)
        result["net"] = to_class(AdhocMargin, self.net)
        result["utilised"] = to_class(Utilised, self.utilised)
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


class MarginCommodityProperties:
    data: AvailableClass
    status: AdhocMargin

    def __init__(self, data: AvailableClass, status: AdhocMargin) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'MarginCommodityProperties':
        assert isinstance(obj, dict)
        data = AvailableClass.from_dict(obj.get("data"))
        status = AdhocMargin.from_dict(obj.get("status"))
        return MarginCommodityProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(AvailableClass, self.data)
        result["status"] = to_class(AdhocMargin, self.status)
        return result


class MarginCommodityClass:
    additional_properties: bool
    properties: MarginCommodityProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MarginCommodityProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MarginCommodityClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MarginCommodityProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MarginCommodityClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MarginCommodityProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    available: Available
    data: Data
    margin_commodity: MarginCommodityClass

    def __init__(self, available: Available, data: Data, margin_commodity: MarginCommodityClass) -> None:
        self.available = available
        self.data = data
        self.margin_commodity = margin_commodity

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        available = Available.from_dict(obj.get("Available"))
        data = Data.from_dict(obj.get("Data"))
        margin_commodity = MarginCommodityClass.from_dict(obj.get("MarginCommodity"))
        return Definitions(available, data, margin_commodity)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Available"] = to_class(Available, self.available)
        result["Data"] = to_class(Data, self.data)
        result["MarginCommodity"] = to_class(MarginCommodityClass, self.margin_commodity)
        return result


class MarginCommodity:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'MarginCommodity':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return MarginCommodity(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def margin_commodity_from_dict(s: Any) -> MarginCommodity:
    return MarginCommodity.from_dict(s)


def margin_commodity_to_dict(x: MarginCommodity) -> Any:
    return to_class(MarginCommodity, x)
