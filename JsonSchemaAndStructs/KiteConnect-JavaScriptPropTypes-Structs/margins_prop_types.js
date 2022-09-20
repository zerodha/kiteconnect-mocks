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

let _Margins;
let _Data;
let _Ity;
let _Available;
_Available = PropTypes.shape({
    "adhoc_margin": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "cash": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "collateral": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "intraday_payin": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "live_balance": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "opening_balance": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Ity = PropTypes.shape({
    "available": PropTypes.oneOfType([_Available, PropTypes.any]),
    "enabled": PropTypes.oneOfType([PropTypes.bool, PropTypes.any]),
    "net": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "utilised": PropTypes.oneOfType([PropTypes.object, PropTypes.any]),
});
_Data = PropTypes.shape({
    "commodity": PropTypes.oneOfType([_Ity, PropTypes.any]),
    "equity": PropTypes.oneOfType([_Ity, PropTypes.any]),
});
_Margins = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const Margins = _Margins;
