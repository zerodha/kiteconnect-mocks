// To parse this data:
//
//   const Convert = require("./file");
//
//   const gttGetOrder = Convert.toGttGetOrder(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
function toGttGetOrder(json) {
    return cast(JSON.parse(json), r("GttGetOrder"));
}

function gttGetOrderToJson(value) {
    return JSON.stringify(uncast(value, r("GttGetOrder")), null, 2);
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
    "GttGetOrder": o([
        { json: "data", js: "data", typ: u(undefined, r("Data")) },
        { json: "status", js: "status", typ: u(undefined, "") },
    ], false),
    "Data": o([
        { json: "condition", js: "condition", typ: u(undefined, r("Condition")) },
        { json: "created_at", js: "created_at", typ: u(undefined, Date) },
        { json: "expires_at", js: "expires_at", typ: u(undefined, Date) },
        { json: "id", js: "id", typ: u(undefined, 0) },
        { json: "meta", js: "meta", typ: u(undefined, null) },
        { json: "orders", js: "orders", typ: u(undefined, a(r("Order"))) },
        { json: "parent_trigger", js: "parent_trigger", typ: u(undefined, null) },
        { json: "status", js: "status", typ: u(undefined, "") },
        { json: "type", js: "type", typ: u(undefined, "") },
        { json: "updated_at", js: "updated_at", typ: u(undefined, Date) },
        { json: "user_id", js: "user_id", typ: u(undefined, "") },
    ], false),
    "Condition": o([
        { json: "exchange", js: "exchange", typ: u(undefined, "") },
        { json: "instrument_token", js: "instrument_token", typ: u(undefined, 0) },
        { json: "last_price", js: "last_price", typ: u(undefined, 3.14) },
        { json: "tradingsymbol", js: "tradingsymbol", typ: u(undefined, "") },
        { json: "trigger_values", js: "trigger_values", typ: u(undefined, a(3.14)) },
    ], false),
    "Order": o([
        { json: "exchange", js: "exchange", typ: u(undefined, "") },
        { json: "order_type", js: "order_type", typ: u(undefined, "") },
        { json: "price", js: "price", typ: u(undefined, 0) },
        { json: "product", js: "product", typ: u(undefined, "") },
        { json: "quantity", js: "quantity", typ: u(undefined, 0) },
        { json: "result", js: "result", typ: u(undefined, u(r("Result"), null)) },
        { json: "tradingsymbol", js: "tradingsymbol", typ: u(undefined, "") },
        { json: "transaction_type", js: "transaction_type", typ: u(undefined, "") },
    ], false),
    "Result": o([
        { json: "account_id", js: "account_id", typ: u(undefined, "") },
        { json: "exchange", js: "exchange", typ: u(undefined, "") },
        { json: "meta", js: "meta", typ: u(undefined, "") },
        { json: "order_result", js: "order_result", typ: u(undefined, r("OrderResult")) },
        { json: "order_type", js: "order_type", typ: u(undefined, "") },
        { json: "price", js: "price", typ: u(undefined, 0) },
        { json: "product", js: "product", typ: u(undefined, "") },
        { json: "quantity", js: "quantity", typ: u(undefined, 0) },
        { json: "timestamp", js: "timestamp", typ: u(undefined, Date) },
        { json: "tradingsymbol", js: "tradingsymbol", typ: u(undefined, "") },
        { json: "transaction_type", js: "transaction_type", typ: u(undefined, "") },
        { json: "triggered_at", js: "triggered_at", typ: u(undefined, 3.14) },
        { json: "validity", js: "validity", typ: u(undefined, "") },
    ], false),
    "OrderResult": o([
        { json: "order_id", js: "order_id", typ: u(undefined, "") },
        { json: "rejection_reason", js: "rejection_reason", typ: u(undefined, "") },
        { json: "status", js: "status", typ: u(undefined, "") },
    ], false),
};

module.exports = {
    "gttGetOrderToJson": gttGetOrderToJson,
    "toGttGetOrder": toGttGetOrder,
};
