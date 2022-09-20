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

let _MFHoldings;
let _Datum;
_Datum = PropTypes.shape({
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "folio": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "fund": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price_date": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "pledged_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "pnl": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_MFHoldings = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.arrayOf(_Datum), PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const MFHoldings = _MFHoldings;
