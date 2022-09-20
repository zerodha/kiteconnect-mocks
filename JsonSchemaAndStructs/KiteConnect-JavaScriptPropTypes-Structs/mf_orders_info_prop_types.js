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

let _MFOrdersInfo;
let _Data;
_Data = PropTypes.shape({
    "amount": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "average_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "exchange_order_id": PropTypes.any,
    "exchange_timestamp": PropTypes.any,
    "folio": PropTypes.any,
    "fund": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price_date": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "order_timestamp": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "placed_by": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "purchase_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "settlement_id": PropTypes.any,
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "status_message": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tag": PropTypes.any,
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "variety": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_MFOrdersInfo = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const MFOrdersInfo = _MFOrdersInfo;
