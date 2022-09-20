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

let _MFOrders;
let _Datum;
_Datum = PropTypes.shape({
    "amount": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "exchange_order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchange_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "folio": PropTypes.any,
    "fund": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price_date": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "placed_by": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "purchase_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "settlement_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "status_message": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tag": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "variety": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_MFOrders = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.arrayOf(_Datum), PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const MFOrders = _MFOrders;
