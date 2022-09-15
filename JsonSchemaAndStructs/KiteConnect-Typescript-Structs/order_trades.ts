// To parse this data:
//
//   import { Convert, OrderTrades } from "./file";
//
//   const orderTrades = Convert.toOrderTrades(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

export interface OrderTrades {
    $ref:        string;
    $schema:     string;
    definitions: Definitions;
}

export interface Definitions {
    Datum:       Datum;
    OrderTrades: OrderTradesClass;
}

export interface Datum {
    additionalProperties: boolean;
    properties:           DatumProperties;
    required:             string[];
    title:                string;
    type:                 string;
}

export interface DatumProperties {
    average_price:      AveragePrice;
    exchange:           AveragePrice;
    exchange_order_id:  AveragePrice;
    exchange_timestamp: ExchangeTimestamp;
    fill_timestamp:     ExchangeTimestamp;
    instrument_token:   AveragePrice;
    order_id:           AveragePrice;
    order_timestamp:    ExchangeTimestamp;
    product:            AveragePrice;
    quantity:           AveragePrice;
    trade_id:           ExchangeTimestamp;
    tradingsymbol:      AveragePrice;
    transaction_type:   AveragePrice;
}

export interface AveragePrice {
    type: Type;
}

export enum Type {
    Integer = "integer",
    String = "string",
}

export interface ExchangeTimestamp {
    format: string;
    type:   Type;
}

export interface OrderTradesClass {
    additionalProperties: boolean;
    properties:           OrderTradesProperties;
    required:             string[];
    title:                string;
    type:                 string;
}

export interface OrderTradesProperties {
    data:   Data;
    status: AveragePrice;
}

export interface Data {
    items: Items;
    type:  string;
}

export interface Items {
    $ref: string;
}

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
export class Convert {
    public static toOrderTrades(json: string): OrderTrades {
        return cast(JSON.parse(json), r("OrderTrades"));
    }

    public static orderTradesToJson(value: OrderTrades): string {
        return JSON.stringify(uncast(value, r("OrderTrades")), null, 2);
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
    "OrderTrades": o([
        { json: "$ref", js: "$ref", typ: "" },
        { json: "$schema", js: "$schema", typ: "" },
        { json: "definitions", js: "definitions", typ: r("Definitions") },
    ], false),
    "Definitions": o([
        { json: "Datum", js: "Datum", typ: r("Datum") },
        { json: "OrderTrades", js: "OrderTrades", typ: r("OrderTradesClass") },
    ], false),
    "Datum": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("DatumProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "DatumProperties": o([
        { json: "average_price", js: "average_price", typ: r("AveragePrice") },
        { json: "exchange", js: "exchange", typ: r("AveragePrice") },
        { json: "exchange_order_id", js: "exchange_order_id", typ: r("AveragePrice") },
        { json: "exchange_timestamp", js: "exchange_timestamp", typ: r("ExchangeTimestamp") },
        { json: "fill_timestamp", js: "fill_timestamp", typ: r("ExchangeTimestamp") },
        { json: "instrument_token", js: "instrument_token", typ: r("AveragePrice") },
        { json: "order_id", js: "order_id", typ: r("AveragePrice") },
        { json: "order_timestamp", js: "order_timestamp", typ: r("ExchangeTimestamp") },
        { json: "product", js: "product", typ: r("AveragePrice") },
        { json: "quantity", js: "quantity", typ: r("AveragePrice") },
        { json: "trade_id", js: "trade_id", typ: r("ExchangeTimestamp") },
        { json: "tradingsymbol", js: "tradingsymbol", typ: r("AveragePrice") },
        { json: "transaction_type", js: "transaction_type", typ: r("AveragePrice") },
    ], false),
    "AveragePrice": o([
        { json: "type", js: "type", typ: r("Type") },
    ], false),
    "ExchangeTimestamp": o([
        { json: "format", js: "format", typ: "" },
        { json: "type", js: "type", typ: r("Type") },
    ], false),
    "OrderTradesClass": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("OrderTradesProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "OrderTradesProperties": o([
        { json: "data", js: "data", typ: r("Data") },
        { json: "status", js: "status", typ: r("AveragePrice") },
    ], false),
    "Data": o([
        { json: "items", js: "items", typ: r("Items") },
        { json: "type", js: "type", typ: "" },
    ], false),
    "Items": o([
        { json: "$ref", js: "$ref", typ: "" },
    ], false),
    "Type": [
        "integer",
        "string",
    ],
};
