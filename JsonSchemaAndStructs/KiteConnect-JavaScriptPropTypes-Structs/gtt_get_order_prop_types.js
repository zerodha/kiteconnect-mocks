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

let _GttGetOrder;
let _Data;
let _Condition;
let _Order;
let _Result;
let _OrderResult;
_OrderResult = PropTypes.shape({
    "order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "rejection_reason": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_Result = PropTypes.shape({
    "account_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "meta": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_result": PropTypes.oneOfType([_OrderResult, PropTypes.any]),
    "order_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "product": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "triggered_at": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "validity": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_Order = PropTypes.shape({
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "product": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "result": PropTypes.oneOfType([_Result, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_Condition = PropTypes.shape({
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "trigger_values": PropTypes.oneOfType([PropTypes.arrayOf(PropTypes.number), PropTypes.any]),
});
_Data = PropTypes.shape({
    "condition": PropTypes.oneOfType([_Condition, PropTypes.any]),
    "created_at": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "expires_at": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "id": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "meta": PropTypes.any,
    "orders": PropTypes.oneOfType([PropTypes.arrayOf(_Order), PropTypes.any]),
    "parent_trigger": PropTypes.any,
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "updated_at": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "user_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_GttGetOrder = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const GttGetOrder = _GttGetOrder;
