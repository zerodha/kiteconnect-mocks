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

let _GenerateSession;
let _Data;
let _Meta;
_Meta = PropTypes.shape({
    "demat_consent": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_Data = PropTypes.shape({
    "access_token": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "api_key": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "avatar_url": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "broker": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "email": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "enctoken": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "exchanges": PropTypes.oneOfType([PropTypes.arrayOf(PropTypes.string), PropTypes.any]),
    "login_time": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "meta": PropTypes.oneOfType([_Meta, PropTypes.any]),
    "order_types": PropTypes.oneOfType([PropTypes.arrayOf(PropTypes.string), PropTypes.any]),
    "products": PropTypes.oneOfType([PropTypes.arrayOf(PropTypes.string), PropTypes.any]),
    "public_token": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "refresh_token": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "silo": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "user_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "user_name": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "user_shortname": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "user_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});
_GenerateSession = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const GenerateSession = _GenerateSession;
