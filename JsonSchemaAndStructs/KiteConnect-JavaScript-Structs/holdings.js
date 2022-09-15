// To parse this data:
//
//   const Convert = require("./file");
//
//   const holdings = Convert.toHoldings(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
function toHoldings(json) {
    return cast(JSON.parse(json), r("Holdings"));
}

function holdingsToJson(value) {
    return JSON.stringify(uncast(value, r("Holdings")), null, 2);
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
    "Holdings": o([
        { json: "$ref", js: "$ref", typ: "" },
        { json: "$schema", js: "$schema", typ: "" },
        { json: "definitions", js: "definitions", typ: r("Definitions") },
    ], false),
    "Definitions": o([
        { json: "Datum", js: "Datum", typ: r("Datum") },
        { json: "Holdings", js: "Holdings", typ: r("HoldingsClass") },
    ], false),
    "Datum": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("DatumProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "DatumProperties": o([
        { json: "authorised_date", js: "authorised_date", typ: r("AuthorisedDate") },
        { json: "authorised_quantity", js: "authorised_quantity", typ: r("AuthorisedQuantity") },
        { json: "average_price", js: "average_price", typ: r("AuthorisedQuantity") },
        { json: "close_price", js: "close_price", typ: r("AuthorisedQuantity") },
        { json: "collateral_quantity", js: "collateral_quantity", typ: r("AuthorisedQuantity") },
        { json: "collateral_type", js: "collateral_type", typ: r("AuthorisedQuantity") },
        { json: "day_change", js: "day_change", typ: r("AuthorisedQuantity") },
        { json: "day_change_percentage", js: "day_change_percentage", typ: r("AuthorisedQuantity") },
        { json: "discrepancy", js: "discrepancy", typ: r("AuthorisedQuantity") },
        { json: "exchange", js: "exchange", typ: r("AuthorisedQuantity") },
        { json: "instrument_token", js: "instrument_token", typ: r("AuthorisedQuantity") },
        { json: "isin", js: "isin", typ: r("AuthorisedQuantity") },
        { json: "last_price", js: "last_price", typ: r("AuthorisedQuantity") },
        { json: "opening_quantity", js: "opening_quantity", typ: r("AuthorisedQuantity") },
        { json: "pnl", js: "pnl", typ: r("AuthorisedQuantity") },
        { json: "price", js: "price", typ: r("AuthorisedQuantity") },
        { json: "product", js: "product", typ: r("AuthorisedQuantity") },
        { json: "quantity", js: "quantity", typ: r("AuthorisedQuantity") },
        { json: "realised_quantity", js: "realised_quantity", typ: r("AuthorisedQuantity") },
        { json: "t1_quantity", js: "t1_quantity", typ: r("AuthorisedQuantity") },
        { json: "tradingsymbol", js: "tradingsymbol", typ: r("AuthorisedQuantity") },
        { json: "used_quantity", js: "used_quantity", typ: r("AuthorisedQuantity") },
    ], false),
    "AuthorisedDate": o([
        { json: "format", js: "format", typ: "" },
        { json: "type", js: "type", typ: r("Type") },
    ], false),
    "AuthorisedQuantity": o([
        { json: "type", js: "type", typ: r("Type") },
    ], false),
    "HoldingsClass": o([
        { json: "additionalProperties", js: "additionalProperties", typ: true },
        { json: "properties", js: "properties", typ: r("HoldingsProperties") },
        { json: "required", js: "required", typ: a("") },
        { json: "title", js: "title", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], false),
    "HoldingsProperties": o([
        { json: "data", js: "data", typ: r("Data") },
        { json: "status", js: "status", typ: r("AuthorisedQuantity") },
    ], false),
    "Data": o([
        { json: "items", js: "items", typ: r("Items") },
        { json: "type", js: "type", typ: "" },
    ], false),
    "Items": o([
        { json: "$ref", js: "$ref", typ: "" },
    ], false),
    "Type": [
        "boolean",
        "integer",
        "number",
        "string",
    ],
};

module.exports = {
    "holdingsToJson": holdingsToJson,
    "toHoldings": toHoldings,
};
