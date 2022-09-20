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

let _Postback;
let _Meta;
_Meta = PropTypes.shape({
});
_Postback = PropTypes.shape({
    "app_id": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "cancelled_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "checksum": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "disclosed_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "exchange": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange_order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange_update_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "filled_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "guid": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "market_protection": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "meta": PropTypes.oneOfType([_Meta, PropTypes.any]),
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
    "status_message_raw": PropTypes.any,
    "tag": PropTypes.any,
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "trigger_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "unfilled_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "user_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "validity": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "variety": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const Postback = _Postback;
