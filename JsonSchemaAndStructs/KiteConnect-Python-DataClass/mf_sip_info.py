# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = mfsip_info_from_dict(json.loads(json_string))

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
    format: str
    type: TypeEnum

    def __init__(self, format: str, type: TypeEnum) -> None:
        self.format = format
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Created':
        assert isinstance(obj, dict)
        format = from_str(obj.get("format"))
        type = TypeEnum(obj.get("type"))
        return Created(format, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["format"] = from_str(self.format)
        result["type"] = to_enum(TypeEnum, self.type)
        return result


class StepUp:
    ref: str

    def __init__(self, ref: str) -> None:
        self.ref = ref

    @staticmethod
    def from_dict(obj: Any) -> 'StepUp':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        return StepUp(ref)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        return result


class DataProperties:
    completed_instalments: CompletedInstalments
    created: Created
    dividend_type: CompletedInstalments
    frequency: CompletedInstalments
    fund: CompletedInstalments
    fund_source: CompletedInstalments
    instalment_amount: CompletedInstalments
    instalment_day: CompletedInstalments
    instalments: CompletedInstalments
    last_instalment: Created
    next_instalment: Created
    pending_instalments: CompletedInstalments
    sip_id: CompletedInstalments
    sip_reg_num: CompletedInstalments
    sip_type: CompletedInstalments
    status: CompletedInstalments
    step_up: StepUp
    tag: CompletedInstalments
    tradingsymbol: CompletedInstalments
    transaction_type: CompletedInstalments
    trigger_price: CompletedInstalments

    def __init__(self, completed_instalments: CompletedInstalments, created: Created, dividend_type: CompletedInstalments, frequency: CompletedInstalments, fund: CompletedInstalments, fund_source: CompletedInstalments, instalment_amount: CompletedInstalments, instalment_day: CompletedInstalments, instalments: CompletedInstalments, last_instalment: Created, next_instalment: Created, pending_instalments: CompletedInstalments, sip_id: CompletedInstalments, sip_reg_num: CompletedInstalments, sip_type: CompletedInstalments, status: CompletedInstalments, step_up: StepUp, tag: CompletedInstalments, tradingsymbol: CompletedInstalments, transaction_type: CompletedInstalments, trigger_price: CompletedInstalments) -> None:
        self.completed_instalments = completed_instalments
        self.created = created
        self.dividend_type = dividend_type
        self.frequency = frequency
        self.fund = fund
        self.fund_source = fund_source
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
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        completed_instalments = CompletedInstalments.from_dict(obj.get("completed_instalments"))
        created = Created.from_dict(obj.get("created"))
        dividend_type = CompletedInstalments.from_dict(obj.get("dividend_type"))
        frequency = CompletedInstalments.from_dict(obj.get("frequency"))
        fund = CompletedInstalments.from_dict(obj.get("fund"))
        fund_source = CompletedInstalments.from_dict(obj.get("fund_source"))
        instalment_amount = CompletedInstalments.from_dict(obj.get("instalment_amount"))
        instalment_day = CompletedInstalments.from_dict(obj.get("instalment_day"))
        instalments = CompletedInstalments.from_dict(obj.get("instalments"))
        last_instalment = Created.from_dict(obj.get("last_instalment"))
        next_instalment = Created.from_dict(obj.get("next_instalment"))
        pending_instalments = CompletedInstalments.from_dict(obj.get("pending_instalments"))
        sip_id = CompletedInstalments.from_dict(obj.get("sip_id"))
        sip_reg_num = CompletedInstalments.from_dict(obj.get("sip_reg_num"))
        sip_type = CompletedInstalments.from_dict(obj.get("sip_type"))
        status = CompletedInstalments.from_dict(obj.get("status"))
        step_up = StepUp.from_dict(obj.get("step_up"))
        tag = CompletedInstalments.from_dict(obj.get("tag"))
        tradingsymbol = CompletedInstalments.from_dict(obj.get("tradingsymbol"))
        transaction_type = CompletedInstalments.from_dict(obj.get("transaction_type"))
        trigger_price = CompletedInstalments.from_dict(obj.get("trigger_price"))
        return DataProperties(completed_instalments, created, dividend_type, frequency, fund, fund_source, instalment_amount, instalment_day, instalments, last_instalment, next_instalment, pending_instalments, sip_id, sip_reg_num, sip_type, status, step_up, tag, tradingsymbol, transaction_type, trigger_price)

    def to_dict(self) -> dict:
        result: dict = {}
        result["completed_instalments"] = to_class(CompletedInstalments, self.completed_instalments)
        result["created"] = to_class(Created, self.created)
        result["dividend_type"] = to_class(CompletedInstalments, self.dividend_type)
        result["frequency"] = to_class(CompletedInstalments, self.frequency)
        result["fund"] = to_class(CompletedInstalments, self.fund)
        result["fund_source"] = to_class(CompletedInstalments, self.fund_source)
        result["instalment_amount"] = to_class(CompletedInstalments, self.instalment_amount)
        result["instalment_day"] = to_class(CompletedInstalments, self.instalment_day)
        result["instalments"] = to_class(CompletedInstalments, self.instalments)
        result["last_instalment"] = to_class(Created, self.last_instalment)
        result["next_instalment"] = to_class(Created, self.next_instalment)
        result["pending_instalments"] = to_class(CompletedInstalments, self.pending_instalments)
        result["sip_id"] = to_class(CompletedInstalments, self.sip_id)
        result["sip_reg_num"] = to_class(CompletedInstalments, self.sip_reg_num)
        result["sip_type"] = to_class(CompletedInstalments, self.sip_type)
        result["status"] = to_class(CompletedInstalments, self.status)
        result["step_up"] = to_class(StepUp, self.step_up)
        result["tag"] = to_class(CompletedInstalments, self.tag)
        result["tradingsymbol"] = to_class(CompletedInstalments, self.tradingsymbol)
        result["transaction_type"] = to_class(CompletedInstalments, self.transaction_type)
        result["trigger_price"] = to_class(CompletedInstalments, self.trigger_price)
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


class MFSIPInfoProperties:
    data: StepUp
    status: CompletedInstalments

    def __init__(self, data: StepUp, status: CompletedInstalments) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'MFSIPInfoProperties':
        assert isinstance(obj, dict)
        data = StepUp.from_dict(obj.get("data"))
        status = CompletedInstalments.from_dict(obj.get("status"))
        return MFSIPInfoProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(StepUp, self.data)
        result["status"] = to_class(CompletedInstalments, self.status)
        return result


class MFSIPInfoClass:
    additional_properties: bool
    properties: MFSIPInfoProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MFSIPInfoProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MFSIPInfoClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MFSIPInfoProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MFSIPInfoClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MFSIPInfoProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class StepUpProperties:
    the_1502: CompletedInstalments

    def __init__(self, the_1502: CompletedInstalments) -> None:
        self.the_1502 = the_1502

    @staticmethod
    def from_dict(obj: Any) -> 'StepUpProperties':
        assert isinstance(obj, dict)
        the_1502 = CompletedInstalments.from_dict(obj.get("15-02"))
        return StepUpProperties(the_1502)

    def to_dict(self) -> dict:
        result: dict = {}
        result["15-02"] = to_class(CompletedInstalments, self.the_1502)
        return result


class StepUpClass:
    additional_properties: bool
    properties: StepUpProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: StepUpProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'StepUpClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = StepUpProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return StepUpClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(StepUpProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    mfsip_info: MFSIPInfoClass
    step_up: StepUpClass

    def __init__(self, data: Data, mfsip_info: MFSIPInfoClass, step_up: StepUpClass) -> None:
        self.data = data
        self.mfsip_info = mfsip_info
        self.step_up = step_up

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        mfsip_info = MFSIPInfoClass.from_dict(obj.get("MFSIPInfo"))
        step_up = StepUpClass.from_dict(obj.get("StepUp"))
        return Definitions(data, mfsip_info, step_up)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["MFSIPInfo"] = to_class(MFSIPInfoClass, self.mfsip_info)
        result["StepUp"] = to_class(StepUpClass, self.step_up)
        return result


class MFSIPInfo:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'MFSIPInfo':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return MFSIPInfo(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def mfsip_info_from_dict(s: Any) -> MFSIPInfo:
    return MFSIPInfo.from_dict(s)


def mfsip_info_to_dict(x: MFSIPInfo) -> Any:
    return to_class(MFSIPInfo, x)
