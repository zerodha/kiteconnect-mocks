// To parse this data:
//
//   const Convert = require("./file");
//
//   const orderInfo = Convert.toOrderInfo(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
function toOrderInfo(json) {
    return cast(JSON.parse(json), r("OrderInfo"));
}

function orderInfoToJson(value) {
    return JSON.stringify(uncast(value, r("OrderInfo")), null, 2);
}

function invalidValue(typ, val, key = '') {
    if (key) {
        throw Error(`Invalid value for key "${key}". Expected type ${JSON.stringify(typ)} but got ${JSON.stringify(val)}`);
    }
    throw Error(`Invalid value ${JSON.stringify(val)} for type ${JSON.stringify(typ)}`, );
}

function jsonToJSProps(typ) {
    if (typ.jsonToJS === undefined) {
        const map = {};
        typ.props.forEach((p) => map[p.json] = { key: p.js, typ: p.typ });
        typ.jsonToJS = map;
    }
    return typ.jsonToJS;
}

function jsToJSONProps(typ) {
    if (typ.jsToJSON === undefined) {
        const map = {};
        typ.props.forEach((p) => map[p.js] = { key: p.json, typ: p.typ });
        typ.jsToJSON = map;
    }
    return typ.jsToJSON;
}

function transform(val, typ, getProps, key = '') {
    function transformPrimitive(typ, val) {
        if (typeof typ === typeof val) return val;
        return invalidValue(typ, val, key);
    }

    function transformUnion(typs, val) {
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

    function transformEnum(cases, val) {
        if (cases.indexOf(val) !== -1) return val;
        return invalidValue(cases, val);
    }

    function transformArray(typ, val) {
        // val must be an array with no invalid elements
        if (!Array.isArray(val)) return invalidValue("array", val);
        return val.map(el => transform(el, typ, getProps));
    }

    function transformDate(val) {
        if (val === null) {
            return null;
        }
        const d = new Date(val);
        if (isNaN(d.valueOf())) {
            return invalidValue("Date", val);
        }
        return d;
    }

    function transformObject(props, additional, val) {
        if (val === null || typeof val !== "object" || Array.isArray(val)) {
            return invalidValue("object", val);
        }
        const result = {};
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

function cast(val, typ) {
    return transform(val, typ, jsonToJSProps);
}

function uncast(val, typ) {
    return transform(val, typ, jsToJSONProps);
}

function a(typ) {
    return { arrayItems: typ };
}

function u(...typs) {
    return { unionMembers: typs };
}

function o(props, additional) {
    return { props, additional };
}

function m(additional) {
    return { props: [], additional };
}

function r(name) {
    return { ref: name };
}

const typeMap = {
    "OrderInfo": o([
        { json: "$ref", js: "$ref", typ: "" },
        { json: "$schema", js: "$schema", typ: "" },
        { json: "definitions", js: "definitions", typ: r("Definitions") },
    ], false),
    "Definitions": o([
        { json: "Datum", js: "Datum", typ: r("Datum") },
        { json: "OrderInfo", js: "OrderInfo", typ: r("OrderInfoClass") },
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
        { json: "cancelled_quantity", js: "cancelled_quantity", typ: r("AveragePrice") },
        { json: "disclosed_quantity", js: "disclosed_quantity", typ: r("AveragePrice") },
        { json: "exchange", js: "exchange", typ: r("AveragePrice") },
        { json: "exchange_order_id", js: "exchange_order_id", typ: r("ExchangeOrderID") },
        { json: "exchange_timestamp", js: "exchange_timestamp", typ: r("ExchangeTimestamp") },
        { json: "filled_quantity", js: "filled_quantity", typ: r("AveragePrice") },
        { json: "instrument_token", js: "instrument_token", typ: r("AveragePrice") },
        { json: "order_id", js: "order_id", typ: r("AveragePrice") },
        { json: "order_timestamp", js: "order_timestamp", typ: r("OrderTimestamp") },
        { json: "order_type", js: "order_type", typ: r("AveragePrice") },
        { json: "parent_order_id", js: "parent_order_id", typ: r("AveragePrice") },
        { json: "pending_quantity", js: "pending_quantity", typ: r("AveragePrice") },
        { json: "placed_by", js: "placed_by", typ: r("AveragePrice") },
        { json: "price", js: "price", typ: r("AveragePrice") },
        { json: "product", js: "product", typ: r("AveragePrice") },
        { json: "quantity", js: "quantity", typ: r("AveragePrice") },
        { json: "status", js: "status", typ: r("AveragePrice") },
        { json: "status_message", js: "status_message", typ: r("AveragePrice") },
        { json: "tag", js: "tag", typ: r("AveragePrice") },
        { json: "tradingsymbol", js: "tradingsymbol", typ: r("AveragePrice") },
        { json: "transaction_type", js: "transaction_type", typ: r("AveragePrice") },
        { json: "trigger_price", js: "trigger_price", typ: r("AveragePrice") },
        { json: "validity", js: "validity", typ: r("AveragePrice") },
        { json: "variety", js: "variety", typ: r("AveragePrice") },
    ], false),
    "AveragePrice": o([
        { json: "type", js: "type", typ: r("Type") },
    ], false),
    "ExchangeOrderID": o([
        { json: "anyOf", js: "anyOf", typ: a(r("AveragePrice")) },
    ], false),
    "ExchangeTimestamp": o([
        { json: "anyOf", js: "anyOf", typ: a(r("OrderTimestamp")) },
    ], false),
    "OrderTimestamp": o([
        { json: "format", js: "format", typ: u(undefined, "") },
        { json: "type", js: "type", typ: r("Type") },
    ], false),
    "OrderInfoClass": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("OrderInfoProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "OrderInfoProperties": o([
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
        "null",
        "number",
        "string",
    ],
};

module.exports = {
    "orderInfoToJson": orderInfoToJson,
    "toOrderInfo": toOrderInfo,
};
