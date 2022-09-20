// Example usage:
//
// import { MyShape } from ./myShape.js;
//
// class MyComponent extends React.Component {
//   //
// }
//
// MyComponent.propTypes = {
//   input: MyShape
// };

import PropTypes from "prop-types";

let _OrderMargins;
let _Datum;
let _Pnl;
_Pnl = PropTypes.shape({
    "realised": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "unrealised": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Datum = PropTypes.shape({
    "additional": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "bo": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "cash": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exposure": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "option_premium": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "pnl": PropTypes.oneOfType([_Pnl, PropTypes.any]),
    "span": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "total": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "var": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_OrderMargins = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.arrayOf(_Datum), PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const OrderMargins = _OrderMargins;
