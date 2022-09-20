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

let _Quote;
let _Datum;
let _Depth;
let _Buy;
let _Ohlc;
_Ohlc = PropTypes.shape({
    "close": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "high": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "low": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "open": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Buy = PropTypes.shape({
    "orders": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Depth = PropTypes.shape({
    "buy": PropTypes.oneOfType([PropTypes.arrayOf(_Buy), PropTypes.any]),
    "sell": PropTypes.oneOfType([PropTypes.arrayOf(_Buy), PropTypes.any]),
});
_Datum = PropTypes.shape({
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "buy_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "depth": PropTypes.oneOfType([_Depth, PropTypes.any]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_trade_time": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "lower_circuit_limit": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "net_change": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "ohlc": PropTypes.oneOfType([_Ohlc, PropTypes.any]),
    "oi": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "oi_day_high": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "oi_day_low": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "sell_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "upper_circuit_limit": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "volume": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Quote = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.object, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const Quote = _Quote;
