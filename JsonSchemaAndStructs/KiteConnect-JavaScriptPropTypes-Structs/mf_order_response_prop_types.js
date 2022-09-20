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

let _MFOrderResponse;
let _Data;
_Data = PropTypes.shape({
    "order_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_MFOrderResponse = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const MFOrderResponse = _MFOrderResponse;
