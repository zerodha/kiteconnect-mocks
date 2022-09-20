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

let _TickerFull;
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
_TickerFull = PropTypes.shape({
    "average_traded_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "change": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "depth": PropTypes.oneOfType([_Depth, PropTypes.any]),
    "exchange_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_trade_time": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "last_traded_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "mode": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "ohlc": PropTypes.oneOfType([_Ohlc, PropTypes.any]),
    "oi": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "oi_day_high": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "oi_day_low": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "total_buy_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "total_sell_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradable": PropTypes.oneOfType([PropTypes.bool, PropTypes.any]),
    "volume_traded": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});

export const TickerFull = PropTypes.arrayOf(_TickerFull);
