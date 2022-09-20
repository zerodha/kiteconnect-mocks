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

let _OrderResponse;
let _Data;
_Data = PropTypes.shape({
    "order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_OrderResponse = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const OrderResponse = _OrderResponse;
