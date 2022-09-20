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

let _Positions;
let _Data;
let _Day;
_Day = PropTypes.shape({
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "buy_m2m": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "buy_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "buy_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "buy_value": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "close_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "day_buy_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "day_buy_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "day_buy_value": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "day_sell_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "day_sell_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "day_sell_value": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "m2m": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "multiplier": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "overnight_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "pnl": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "product": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "realised": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "sell_m2m": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "sell_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "sell_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "sell_value": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "unrealised": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "value": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Data = PropTypes.shape({
    "day": PropTypes.oneOfType([PropTypes.arrayOf(_Day), PropTypes.any]),
    "net": PropTypes.oneOfType([PropTypes.arrayOf(_Day), PropTypes.any]),
});
_Positions = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const Positions = _Positions;
