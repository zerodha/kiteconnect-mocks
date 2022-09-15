# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = mf_sips_from_dict(json.loads(json_string))

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
    INTEGER = "integer"
    NUMBER = "number"
    STRING = "string"


class CompletedInstalments:
    type: TypeEnum

    def __init__(self, type: TypeEnum) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'CompletedInstalments':
        assert isinstance(obj, dict)
        type = TypeEnum(obj.get("type"))
        return CompletedInstalments(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class Created:
    format: Optional[str]
    type: str

    def __init__(self, format: Optional[str], type: str) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Created':
        assert isinstance(obj, dict)
        format = from_union([from_str, from_none], obj.get("format"))
        type = from_str(obj.get("type"))
        return Created(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_union([from_str, from_none], self.format)
        result["type"] = from_str(self.type)
        return result


class SIPRegNum:
    any_of: List[Created]

    def __init__(self, any_of: List[Created]) -> None:
        self.any_of = any_of

    @staticmethod
    def from_dict(obj: Any) -> 'SIPRegNum':
        assert isinstance(obj, dict)
        any_of = from_list(Created.from_dict, obj.get("anyOf"))
        return SIPRegNum(any_of)

    def to_dict(self) -> dict:
        result: dict = {}
        result["anyOf"] = from_list(lambda x: to_class(Created, x), self.any_of)
        return result


class StepUp:
    additional_properties: CompletedInstalments
    type: str

    def __init__(self, additional_properties: CompletedInstalments, type: str) -> None:
        self.additional_properties = additional_properties
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'StepUp':
        assert isinstance(obj, dict)
        additional_properties = CompletedInstalments.from_dict(obj.get("additionalProperties"))
        type = from_str(obj.get("type"))
        return StepUp(additional_properties, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = to_class(CompletedInstalments, self.additional_properties)
        result["type"] = from_str(self.type)
        return result


class DatumProperties:
    completed_instalments: CompletedInstalments
    created: Created
    dividend_type: CompletedInstalments
    frequency: CompletedInstalments
    fund: CompletedInstalments
    instalment_amount: CompletedInstalments
    instalment_day: CompletedInstalments
    instalments: CompletedInstalments
    last_instalment: Created
    next_instalment: Created
    pending_instalments: CompletedInstalments
    sip_id: CompletedInstalments
    sip_reg_num: SIPRegNum
    sip_type: CompletedInstalments
    status: CompletedInstalments
    step_up: StepUp
    tag: CompletedInstalments
    tradingsymbol: CompletedInstalments
    transaction_type: CompletedInstalments
    trigger_price: CompletedInstalments

    def __init__(self, completed_instalments: CompletedInstalments, created: Created, dividend_type: CompletedInstalments, frequency: CompletedInstalments, fund: CompletedInstalments, instalment_amount: CompletedInstalments, instalment_day: CompletedInstalments, instalments: CompletedInstalments, last_instalment: Created, next_instalment: Created, pending_instalments: CompletedInstalments, sip_id: CompletedInstalments, sip_reg_num: SIPRegNum, sip_type: CompletedInstalments, status: CompletedInstalments, step_up: StepUp, tag: CompletedInstalments, tradingsymbol: CompletedInstalments, transaction_type: CompletedInstalments, trigger_price: CompletedInstalments) -> None:
        self.completed_instalments = completed_instalments
        self.created = created
        self.dividend_type = dividend_type
        self.frequency = frequency
        self.fund = fund
        self.instalment_amount = instalment_amount
        self.instalment_day = instalment_day
        self.instalments = instalments
        self.last_instalment = last_instalment
        self.next_instalment = next_instalment
        self.pending_instalments = pending_instalments
        self.sip_id = sip_id
        self.sip_reg_num = sip_reg_num
        self.sip_type = sip_type
        self.status = status
        self.step_up = step_up
        self.tag = tag
        self.tradingsymbol = tradingsymbol
        self.transaction_type = transaction_type
        self.trigger_price = trigger_price

    @staticmethod
    def from_dict(obj: Any) -> 'DatumProperties':
        assert isinstance(obj, dict)
        completed_instalments = CompletedInstalments.from_dict(obj.get("completed_instalments"))
        created = Created.from_dict(obj.get("created"))
        dividend_type = CompletedInstalments.from_dict(obj.get("dividend_type"))
        frequency = CompletedInstalments.from_dict(obj.get("frequency"))
        fund = CompletedInstalments.from_dict(obj.get("fund"))
        instalment_amount = CompletedInstalments.from_dict(obj.get("instalment_amount"))
        instalment_day = CompletedInstalments.from_dict(obj.get("instalment_day"))
        instalments = CompletedInstalments.from_dict(obj.get("instalments"))
        last_instalment = Created.from_dict(obj.get("last_instalment"))
        next_instalment = Created.from_dict(obj.get("next_instalment"))
        pending_instalments = CompletedInstalments.from_dict(obj.get("pending_instalments"))
        sip_id = CompletedInstalments.from_dict(obj.get("sip_id"))
        sip_reg_num = SIPRegNum.from_dict(obj.get("sip_reg_num"))
        sip_type = CompletedInstalments.from_dict(obj.get("sip_type"))
        status = CompletedInstalments.from_dict(obj.get("status"))
        step_up = StepUp.from_dict(obj.get("step_up"))
        tag = CompletedInstalments.from_dict(obj.get("tag"))
        tradingsymbol = CompletedInstalments.from_dict(obj.get("tradingsymbol"))
        transaction_type = CompletedInstalments.from_dict(obj.get("transaction_type"))
        trigger_price = CompletedInstalments.from_dict(obj.get("trigger_price"))
        return DatumProperties(completed_instalments, created, dividend_type, frequency, fund, instalment_amount, instalment_day, instalments, last_instalment, next_instalment, pending_instalments, sip_id, sip_reg_num, sip_type, status, step_up, tag, tradingsymbol, transaction_type, trigger_price)

    def to_dict(self) -> dict:
        result: dict = {}
        result["completed_instalments"] = to_class(CompletedInstalments, self.completed_instalments)
        result["created"] = to_class(Created, self.created)
        result["dividend_type"] = to_class(CompletedInstalments, self.dividend_type)
        result["frequency"] = to_class(CompletedInstalments, self.frequency)
        result["fund"] = to_class(CompletedInstalments, self.fund)
        result["instalment_amount"] = to_class(CompletedInstalments, self.instalment_amount)
        result["instalment_day"] = to_class(CompletedInstalments, self.instalment_day)
        result["instalments"] = to_class(CompletedInstalments, self.instalments)
        result["last_instalment"] = to_class(Created, self.last_instalment)
        result["next_instalment"] = to_class(Created, self.next_instalment)
        result["pending_instalments"] = to_class(CompletedInstalments, self.pending_instalments)
        result["sip_id"] = to_class(CompletedInstalments, self.sip_id)
        result["sip_reg_num"] = to_class(SIPRegNum, self.sip_reg_num)
        result["sip_type"] = to_class(CompletedInstalments, self.sip_type)
        result["status"] = to_class(CompletedInstalments, self.status)
        result["step_up"] = to_class(StepUp, self.step_up)
        result["tag"] = to_class(CompletedInstalments, self.tag)
        result["tradingsymbol"] = to_class(CompletedInstalments, self.tradingsymbol)
        result["transaction_type"] = to_class(CompletedInstalments, self.transaction_type)
        result["trigger_price"] = to_class(CompletedInstalments, self.trigger_price)
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


class MFSipsProperties:
    data: Data

    def __init__(self, data: Data) -> None:
        self.data = data

    @staticmethod
    def from_dict(obj: Any) -> 'MFSipsProperties':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("data"))
        return MFSipsProperties(data)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(Data, self.data)
        return result


class MFSipsClass:
    additional_properties: bool
    properties: MFSipsProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MFSipsProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MFSipsClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MFSipsProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MFSipsClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MFSipsProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    datum: Datum
    mf_sips: MFSipsClass

    def __init__(self, datum: Datum, mf_sips: MFSipsClass) -> None:
        self.datum = datum
        self.mf_sips = mf_sips

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        datum = Datum.from_dict(obj.get("Datum"))
        mf_sips = MFSipsClass.from_dict(obj.get("MFSips"))
        return Definitions(datum, mf_sips)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Datum"] = to_class(Datum, self.datum)
        result["MFSips"] = to_class(MFSipsClass, self.mf_sips)
        return result


class MFSips:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'MFSips':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return MFSips(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def mf_sips_from_dict(s: Any) -> MFSips:
    return MFSips.from_dict(s)


def mf_sips_to_dict(x: MFSips) -> Any:
    return to_class(MFSips, x)
