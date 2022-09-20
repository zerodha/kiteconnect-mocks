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

let _TickerQuote;
let _Ohlc;
_Ohlc = PropTypes.shape({
    "close": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "high": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "low": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "open": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_TickerQuote = PropTypes.shape({
    "average_traded_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "change": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_traded_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "mode": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "ohlc": PropTypes.oneOfType([_Ohlc, PropTypes.any]),
    "total_buy_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "total_sell_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradable": PropTypes.oneOfType([PropTypes.bool, PropTypes.any]),
    "volume_traded": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});

export const TickerQuote = PropTypes.arrayOf(_TickerQuote);
