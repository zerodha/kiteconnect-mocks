// To parse this data:
//
//   const Convert = require("./file");
//
//   const positions = Convert.toPositions(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
function toPositions(json) {
    return cast(JSON.parse(json), r("Positions"));
}

function positionsToJson(value) {
    return JSON.stringify(uncast(value, r("Positions")), null, 2);
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
    "Positions": o([
        { json: "data", js: "data", typ: u(undefined, r("Data")) },
        { json: "status", js: "status", typ: u(undefined, "") },
    ], false),
    "Data": o([
        { json: "day", js: "day", typ: u(undefined, a(r("Day"))) },
        { json: "net", js: "net", typ: u(undefined, a(r("Day"))) },
    ], false),
    "Day": o([
        { json: "average_price", js: "average_price", typ: u(undefined, 3.14) },
        { json: "buy_m2m", js: "buy_m2m", typ: u(undefined, 0) },
        { json: "buy_price", js: "buy_price", typ: u(undefined, 3.14) },
        { json: "buy_quantity", js: "buy_quantity", typ: u(undefined, 0) },
        { json: "buy_value", js: "buy_value", typ: u(undefined, 0) },
        { json: "close_price", js: "close_price", typ: u(undefined, 0) },
        { json: "day_buy_price", js: "day_buy_price", typ: u(undefined, 3.14) },
        { json: "day_buy_quantity", js: "day_buy_quantity", typ: u(undefined, 0) },
        { json: "day_buy_value", js: "day_buy_value", typ: u(undefined, 0) },
        { json: "day_sell_price", js: "day_sell_price", typ: u(undefined, 0) },
        { json: "day_sell_quantity", js: "day_sell_quantity", typ: u(undefined, 0) },
        { json: "day_sell_value", js: "day_sell_value", typ: u(undefined, 0) },
        { json: "exchange", js: "exchange", typ: u(undefined, "") },
        { json: "instrument_token", js: "instrument_token", typ: u(undefined, 0) },
        { json: "last_price", js: "last_price", typ: u(undefined, 3.14) },
        { json: "m2m", js: "m2m", typ: u(undefined, 0) },
        { json: "multiplier", js: "multiplier", typ: u(undefined, 0) },
        { json: "overnight_quantity", js: "overnight_quantity", typ: u(undefined, 0) },
        { json: "pnl", js: "pnl", typ: u(undefined, 0) },
        { json: "product", js: "product", typ: u(undefined, "") },
        { json: "quantity", js: "quantity", typ: u(undefined, 0) },
        { json: "realised", js: "realised", typ: u(undefined, 0) },
        { json: "sell_m2m", js: "sell_m2m", typ: u(undefined, 0) },
        { json: "sell_price", js: "sell_price", typ: u(undefined, 0) },
        { json: "sell_quantity", js: "sell_quantity", typ: u(undefined, 0) },
        { json: "sell_value", js: "sell_value", typ: u(undefined, 0) },
        { json: "tradingsymbol", js: "tradingsymbol", typ: u(undefined, "") },
        { json: "unrealised", js: "unrealised", typ: u(undefined, 0) },
        { json: "value", js: "value", typ: u(undefined, 0) },
    ], false),
};

module.exports = {
    "positionsToJson": positionsToJson,
    "toPositions": toPositions,
};
