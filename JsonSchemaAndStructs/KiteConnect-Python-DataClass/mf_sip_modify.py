# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = mfsip_modify_from_dict(json.loads(json_string))

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


class Sipid:
    type: str

    def __init__(self, type: str) -> None:
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'Sipid':
        assert isinstance(obj, dict)
        type = from_str(obj.get("type"))
        return Sipid(type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["type"] = from_str(self.type)
        return result


class DataProperties:
    sip_id: Sipid

    def __init__(self, sip_id: Sipid) -> None:
        self.sip_id = sip_id

    @staticmethod
    def from_dict(obj: Any) -> 'DataProperties':
        assert isinstance(obj, dict)
        sip_id = Sipid.from_dict(obj.get("sip_id"))
        return DataProperties(sip_id)

    def to_dict(self) -> dict:
        result: dict = {}
        result["sip_id"] = to_class(Sipid, self.sip_id)
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


class MFSIPModifyProperties:
    data: DataClass
    status: Sipid

    def __init__(self, data: DataClass, status: Sipid) -> None:
        self.data = data
        self.status = status

    @staticmethod
    def from_dict(obj: Any) -> 'MFSIPModifyProperties':
        assert isinstance(obj, dict)
        data = DataClass.from_dict(obj.get("data"))
        status = Sipid.from_dict(obj.get("status"))
        return MFSIPModifyProperties(data, status)

    def to_dict(self) -> dict:
        result: dict = {}
        result["data"] = to_class(DataClass, self.data)
        result["status"] = to_class(Sipid, self.status)
        return result


class MFSIPModifyClass:
    additional_properties: bool
    properties: MFSIPModifyProperties
    required: List[str]
    title: str
    type: str

    def __init__(self, additional_properties: bool, properties: MFSIPModifyProperties, required: List[str], title: str, type: str) -> None:
        self.additional_properties = additional_properties
        self.properties = properties
        self.required = required
        self.title = title
        self.type = type

    @staticmethod
    def from_dict(obj: Any) -> 'MFSIPModifyClass':
        assert isinstance(obj, dict)
        additional_properties = from_bool(obj.get("additionalProperties"))
        properties = MFSIPModifyProperties.from_dict(obj.get("properties"))
        required = from_list(from_str, obj.get("required"))
        title = from_str(obj.get("title"))
        type = from_str(obj.get("type"))
        return MFSIPModifyClass(additional_properties, properties, required, title, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["additionalProperties"] = from_bool(self.additional_properties)
        result["properties"] = to_class(MFSIPModifyProperties, self.properties)
        result["required"] = from_list(from_str, self.required)
        result["title"] = from_str(self.title)
        result["type"] = from_str(self.type)
        return result


class Definitions:
    data: Data
    mfsip_modify: MFSIPModifyClass

    def __init__(self, data: Data, mfsip_modify: MFSIPModifyClass) -> None:
        self.data = data
        self.mfsip_modify = mfsip_modify

    @staticmethod
    def from_dict(obj: Any) -> 'Definitions':
        assert isinstance(obj, dict)
        data = Data.from_dict(obj.get("Data"))
        mfsip_modify = MFSIPModifyClass.from_dict(obj.get("MFSIPModify"))
        return Definitions(data, mfsip_modify)

    def to_dict(self) -> dict:
        result: dict = {}
        result["Data"] = to_class(Data, self.data)
        result["MFSIPModify"] = to_class(MFSIPModifyClass, self.mfsip_modify)
        return result


class MFSIPModify:
    ref: str
    schema: str
    definitions: Definitions

    def __init__(self, ref: str, schema: str, definitions: Definitions) -> None:
        self.ref = ref
        self.schema = schema
        self.definitions = definitions

    @staticmethod
    def from_dict(obj: Any) -> 'MFSIPModify':
        assert isinstance(obj, dict)
        ref = from_str(obj.get("$ref"))
        schema = from_str(obj.get("$schema"))
        definitions = Definitions.from_dict(obj.get("definitions"))
        return MFSIPModify(ref, schema, definitions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["$ref"] = from_str(self.ref)
        result["$schema"] = from_str(self.schema)
        result["definitions"] = to_class(Definitions, self.definitions)
        return result


def mfsip_modify_from_dict(s: Any) -> MFSIPModify:
    return MFSIPModify.from_dict(s)


def mfsip_modify_to_dict(x: MFSIPModify) -> Any:
    return to_class(MFSIPModify, x)
