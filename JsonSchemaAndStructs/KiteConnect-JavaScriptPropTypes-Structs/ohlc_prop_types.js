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

let _Ohlc;
let _Datum;
let _OhlcClass;
_OhlcClass = PropTypes.shape({
    "close": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "high": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "low": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "open": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Datum = PropTypes.shape({
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "ohlc": PropTypes.oneOfType([_OhlcClass, PropTypes.any]),
});
_Ohlc = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.object, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const Ohlc = _Ohlc;
