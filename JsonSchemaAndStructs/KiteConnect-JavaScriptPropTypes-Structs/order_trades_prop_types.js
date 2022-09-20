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

let _OrderTrades;
let _Datum;
_Datum = PropTypes.shape({
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange_order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "fill_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "product": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "trade_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_OrderTrades = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.arrayOf(_Datum), PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const OrderTrades = _OrderTrades;
