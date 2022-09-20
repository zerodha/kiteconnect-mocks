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

let _OrderInfo;
let _Datum;
_Datum = PropTypes.shape({
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "cancelled_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "disclosed_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange_order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "filled_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "parent_order_id": PropTypes.any,
    "pending_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "placed_by": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "product": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "status_message": PropTypes.any,
    "tag": PropTypes.any,
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "trigger_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "validity": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "variety": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_OrderInfo = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.arrayOf(_Datum), PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const OrderInfo = _OrderInfo;
