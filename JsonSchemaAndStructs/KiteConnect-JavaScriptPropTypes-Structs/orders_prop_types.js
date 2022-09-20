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

let _Orders;
let _Datum;
let _Meta;
let _Iceberg;
_Iceberg = PropTypes.shape({
    "leg": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "leg_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "legs": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "remaining_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "total_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Meta = PropTypes.shape({
    "iceberg": PropTypes.oneOfType([_Iceberg, PropTypes.any]),
});
_Datum = PropTypes.shape({
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "cancelled_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
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
    "modified": PropTypes.oneOfType([PropTypes.bool, PropTypes.any]),
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
    "status_message": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "status_message_raw": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tag": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tags": PropTypes.oneOfType([PropTypes.arrayOf(PropTypes.string), PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "trigger_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "validity": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "validity_ttl": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "variety": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_Orders = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.arrayOf(_Datum), PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const Orders = _Orders;
