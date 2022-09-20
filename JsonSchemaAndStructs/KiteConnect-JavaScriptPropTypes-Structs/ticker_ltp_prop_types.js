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

let _TickerLtp;
_TickerLtp = PropTypes.shape({
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "mode": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tradable": PropTypes.oneOfType([PropTypes.bool, PropTypes.any]),
});

export const TickerLtp = PropTypes.arrayOf(_TickerLtp);
