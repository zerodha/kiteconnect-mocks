// To parse this data:
//
//   import { Convert, GttGetOrder } from "./file";
//
//   const gttGetOrder = Convert.toGttGetOrder(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

export interface GttGetOrder {
    $ref:        string;
    $schema:     string;
    definitions: Definitions;
}

export interface Definitions {
    Condition:   Condition;
    Data:        Data;
    GttGetOrder: GttGetOrderClass;
    Order:       Order;
    OrderResult: OrderResult;
    Result:      ResultClass;
}

export interface Condition {
    additionalProperties: boolean;
    properties:           ConditionProperties;
    required:             string[];
    title:                string;
    type:                 string;
}

export interface ConditionProperties {
    exchange:         Exchange;
    instrument_token: Exchange;
    last_price:       Exchange;
    tradingsymbol:    Exchange;
    trigger_values:   TriggerValues;
}

export interface Exchange {
    type: Type;
}

export enum Type {
    Integer = "integer",
    Null = "null",
    Number = "number",
    String = "string",
}

export interface TriggerValues {
    items: Exchange;
    type:  string;
}

export interface Data {
    additionalProperties: boolean;
    properties:           DataProperties;
    required:             string[];
    title:                string;
    type:                 string;
}

export interface DataProperties {
    condition:      ConditionClass;
    created_at:     CreatedAt;
    expires_at:     CreatedAt;
    id:             Exchange;
    meta:           Exchange;
    orders:         Orders;
    parent_trigger: Exchange;
    status:         Exchange;
    type:           Exchange;
    updated_at:     CreatedAt;
    user_id:        Exchange;
}

export interface ConditionClass {
    $ref: string;
}

export interface CreatedAt {
    format: string;
    type:   Type;
}

export interface Orders {
    items: ConditionClass;
    type:  string;
}

export interface GttGetOrderClass {
    additionalProperties: boolean;
    properties:           GttGetOrderProperties;
    required:             string[];
    title:                string;
    type:                 string;
}

export interface GttGetOrderProperties {
    data:   ConditionClass;
    status: Exchange;
}

export interface Order {
    additionalProperties: boolean;
    properties:           OrderProperties;
    required:             string[];
    title:                string;
    type:                 string;
}

export interface OrderProperties {
    exchange:         Exchange;
    order_type:       Exchange;
    price:            Exchange;
    product:          Exchange;
    quantity:         Exchange;
    result:           Result;
    tradingsymbol:    Exchange;
    transaction_type: Exchange;
}

export interface Result {
    anyOf: AnyOf[];
}

export interface AnyOf {
    $ref?: string;
    type?: Type;
}

export interface OrderResult {
    additionalProperties: boolean;
    properties:           OrderResultProperties;
    required:             string[];
    title:                string;
    type:                 string;
}

export interface OrderResultProperties {
    order_id:         Exchange;
    rejection_reason: Exchange;
    status:           Exchange;
}

export interface ResultClass {
    additionalProperties: boolean;
    properties:           ResultProperties;
    required:             string[];
    title:                string;
    type:                 string;
}

export interface ResultProperties {
    account_id:       Exchange;
    exchange:         Exchange;
    meta:             Exchange;
    order_result:     ConditionClass;
    order_type:       Exchange;
    price:            Exchange;
    product:          Exchange;
    quantity:         Exchange;
    timestamp:        CreatedAt;
    tradingsymbol:    Exchange;
    transaction_type: Exchange;
    triggered_at:     Exchange;
    validity:         Exchange;
}

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
export class Convert {
    public static toGttGetOrder(json: string): GttGetOrder {
        return cast(JSON.parse(json), r("GttGetOrder"));
    }

    public static gttGetOrderToJson(value: GttGetOrder): string {
        return JSON.stringify(uncast(value, r("GttGetOrder")), null, 2);
    }
}

function invalidValue(typ: any, val: any, key: any = ''): never {
    if (key) {
        throw Error(`Invalid value for key "${key}". Expected type ${JSON.stringify(typ)} but got ${JSON.stringify(val)}`);
    }
    throw Error(`Invalid value ${JSON.stringify(val)} for type ${JSON.stringify(typ)}`, );
}

function jsonToJSProps(typ: any): any {
    if (typ.jsonToJS === undefined) {
        const map: any = {};
        typ.props.forEach((p: any) => map[p.json] = { key: p.js, typ: p.typ });
        typ.jsonToJS = map;
    }
    return typ.jsonToJS;
}

function jsToJSONProps(typ: any): any {
    if (typ.jsToJSON === undefined) {
        const map: any = {};
        typ.props.forEach((p: any) => map[p.js] = { key: p.json, typ: p.typ });
        typ.jsToJSON = map;
    }
    return typ.jsToJSON;
}

function transform(val: any, typ: any, getProps: any, key: any = ''): any {
    function transformPrimitive(typ: string, val: any): any {
        if (typeof typ === typeof val) return val;
        return invalidValue(typ, val, key);
    }

    function transformUnion(typs: any[], val: any): any {
        // val must validate against one typ in typs
        const l = typs.length;
        for (let i = 0; i < l; i++) {
            const typ = typs[i];
            try {
                return transform(val, typ, getProps);
            } catch (_) {}
        }
        return invalidValue(typs, val);
    }

    function transformEnum(cases: string[], val: any): any {
        if (cases.indexOf(val) !== -1) return val;
        return invalidValue(cases, val);
    }

    function transformArray(typ: any, val: any): any {
        // val must be an array with no invalid elements
        if (!Array.isArray(val)) return invalidValue("array", val);
        return val.map(el => transform(el, typ, getProps));
    }

    function transformDate(val: any): any {
        if (val === null) {
            return null;
        }
        const d = new Date(val);
        if (isNaN(d.valueOf())) {
            return invalidValue("Date", val);
        }
        return d;
    }

    function transformObject(props: { [k: string]: any }, additional: any, val: any): any {
        if (val === null || typeof val !== "object" || Array.isArray(val)) {
            return invalidValue("object", val);
        }
        const result: any = {};
        Object.getOwnPropertyNames(props).forEach(key => {
            const prop = props[key];
            const v = Object.prototype.hasOwnProperty.call(val, key) ? val[key] : undefined;
            result[prop.key] = transform(v, prop.typ, getProps, prop.key);
        });
        Object.getOwnPropertyNames(val).forEach(key => {
            if (!Object.prototype.hasOwnProperty.call(props, key)) {
                result[key] = transform(val[key], additional, getProps, key);
            }
        });
        return result;
    }

    if (typ === "any") return val;
    if (typ === null) {
        if (val === null) return val;
        return invalidValue(typ, val);
    }
    if (typ === false) return invalidValue(typ, val);
    while (typeof typ === "object" && typ.ref !== undefined) {
        typ = typeMap[typ.ref];
    }
    if (Array.isArray(typ)) return transformEnum(typ, val);
    if (typeof typ === "object") {
        return typ.hasOwnProperty("unionMembers") ? transformUnion(typ.unionMembers, val)
            : typ.hasOwnProperty("arrayItems")    ? transformArray(typ.arrayItems, val)
            : typ.hasOwnProperty("props")         ? transformObject(getProps(typ), typ.additional, val)
            : invalidValue(typ, val);
    }
    // Numbers can be parsed by Date but shouldn't be.
    if (typ === Date && typeof val !== "number") return transformDate(val);
    return transformPrimitive(typ, val);
}

function cast<T>(val: any, typ: any): T {
    return transform(val, typ, jsonToJSProps);
}

function uncast<T>(val: T, typ: any): any {
    return transform(val, typ, jsToJSONProps);
}

function a(typ: any) {
    return { arrayItems: typ };
}

function u(...typs: any[]) {
    return { unionMembers: typs };
}

function o(props: any[], additional: any) {
    return { props, additional };
}

function m(additional: any) {
    return { props: [], additional };
}

function r(name: string) {
    return { ref: name };
}

const typeMap: any = {
    "GttGetOrder": o([
        { json: "$ref", js: "$ref", typ: "" },
        { json: "$schema", js: "$schema", typ: "" },
        { json: "definitions", js: "definitions", typ: r("Definitions") },
    ], false),
    "Definitions": o([
        { json: "Condition", js: "Condition", typ: r("Condition") },
        { json: "Data", js: "Data", typ: r("Data") },
        { json: "GttGetOrder", js: "GttGetOrder", typ: r("GttGetOrderClass") },
        { json: "Order", js: "Order", typ: r("Order") },
        { json: "OrderResult", js: "OrderResult", typ: r("OrderResult") },
        { json: "Result", js: "Result", typ: r("ResultClass") },
    ], false),
    "Condition": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("ConditionProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "ConditionProperties": o([
        { json: "exchange", js: "exchange", typ: r("Exchange") },
        { json: "instrument_token", js: "instrument_token", typ: r("Exchange") },
        { json: "last_price", js: "last_price", typ: r("Exchange") },
        { json: "tradingsymbol", js: "tradingsymbol", typ: r("Exchange") },
        { json: "trigger_values", js: "trigger_values", typ: r("TriggerValues") },
    ], false),
    "Exchange": o([
        { json: "type", js: "type", typ: r("Type") },
    ], false),
    "TriggerValues": o([
        { json: "items", js: "items", typ: r("Exchange") },
        { json: "type", js: "type", typ: "" },
    ], false),
    "Data": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("DataProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "DataProperties": o([
        { json: "condition", js: "condition", typ: r("ConditionClass") },
        { json: "created_at", js: "created_at", typ: r("CreatedAt") },
        { json: "expires_at", js: "expires_at", typ: r("CreatedAt") },
        { json: "id", js: "id", typ: r("Exchange") },
        { json: "meta", js: "meta", typ: r("Exchange") },
        { json: "orders", js: "orders", typ: r("Orders") },
        { json: "parent_trigger", js: "parent_trigger", typ: r("Exchange") },
        { json: "status", js: "status", typ: r("Exchange") },
        { json: "type", js: "type", typ: r("Exchange") },
        { json: "updated_at", js: "updated_at", typ: r("CreatedAt") },
        { json: "user_id", js: "user_id", typ: r("Exchange") },
    ], false),
    "ConditionClass": o([
        { json: "$ref", js: "$ref", typ: "" },
    ], false),
    "CreatedAt": o([
        { json: "format", js: "format", typ: "" },
        { json: "type", js: "type", typ: r("Type") },
    ], false),
    "Orders": o([
        { json: "items", js: "items", typ: r("ConditionClass") },
        { json: "type", js: "type", typ: "" },
    ], false),
    "GttGetOrderClass": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("GttGetOrderProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "GttGetOrderProperties": o([
        { json: "data", js: "data", typ: r("ConditionClass") },
        { json: "status", js: "status", typ: r("Exchange") },
    ], false),
    "Order": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("OrderProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "OrderProperties": o([
        { json: "exchange", js: "exchange", typ: r("Exchange") },
        { json: "order_type", js: "order_type", typ: r("Exchange") },
        { json: "price", js: "price", typ: r("Exchange") },
        { json: "product", js: "product", typ: r("Exchange") },
        { json: "quantity", js: "quantity", typ: r("Exchange") },
        { json: "result", js: "result", typ: r("Result") },
        { json: "tradingsymbol", js: "tradingsymbol", typ: r("Exchange") },
        { json: "transaction_type", js: "transaction_type", typ: r("Exchange") },
    ], false),
    "Result": o([
        { json: "anyOf", js: "anyOf", typ: a(r("AnyOf")) },
    ], false),
    "AnyOf": o([
        { json: "$ref", js: "$ref", typ: u(undefined, "") },
        { json: "type", js: "type", typ: u(undefined, r("Type")) },
    ], false),
    "OrderResult": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("OrderResultProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "OrderResultProperties": o([
        { json: "order_id", js: "order_id", typ: r("Exchange") },
        { json: "rejection_reason", js: "rejection_reason", typ: r("Exchange") },
        { json: "status", js: "status", typ: r("Exchange") },
    ], false),
    "ResultClass": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("ResultProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "ResultProperties": o([
        { json: "account_id", js: "account_id", typ: r("Exchange") },
        { json: "exchange", js: "exchange", typ: r("Exchange") },
        { json: "meta", js: "meta", typ: r("Exchange") },
        { json: "order_result", js: "order_result", typ: r("ConditionClass") },
        { json: "order_type", js: "order_type", typ: r("Exchange") },
        { json: "price", js: "price", typ: r("Exchange") },
        { json: "product", js: "product", typ: r("Exchange") },
        { json: "quantity", js: "quantity", typ: r("Exchange") },
        { json: "timestamp", js: "timestamp", typ: r("CreatedAt") },
        { json: "tradingsymbol", js: "tradingsymbol", typ: r("Exchange") },
        { json: "transaction_type", js: "transaction_type", typ: r("Exchange") },
        { json: "triggered_at", js: "triggered_at", typ: r("Exchange") },
        { json: "validity", js: "validity", typ: r("Exchange") },
    ], false),
    "Type": [
        "integer",
        "null",
        "number",
        "string",
    ],
};
