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

let _Profile;
let _Data;
let _Meta;
_Meta = PropTypes.shape({
    "demat_consent": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_Data = PropTypes.shape({
    "avatar_url": PropTypes.any,
    "broker": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "email": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchanges": PropTypes.oneOfType([PropTypes.arrayOf(PropTypes.string), PropTypes.any]),
    "meta": PropTypes.oneOfType([_Meta, PropTypes.any]),
    "order_types": PropTypes.oneOfType([PropTypes.arrayOf(PropTypes.string), PropTypes.any]),
    "products": PropTypes.oneOfType([PropTypes.arrayOf(PropTypes.string), PropTypes.any]),
    "user_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "user_name": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "user_shortname": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "user_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_Profile = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const Profile = _Profile;
