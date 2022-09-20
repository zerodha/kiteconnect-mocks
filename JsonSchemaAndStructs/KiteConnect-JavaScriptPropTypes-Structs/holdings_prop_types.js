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

let _Holdings;
let _Datum;
_Datum = PropTypes.shape({
    "authorised_date": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "authorised_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "close_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "collateral_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "collateral_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "day_change": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "day_change_percentage": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "discrepancy": PropTypes.oneOfType([PropTypes.bool, PropTypes.any]),
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "isin": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "opening_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "pnl": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "product": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "realised_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "t1_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "used_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Holdings = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.arrayOf(_Datum), PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const Holdings = _Holdings;
