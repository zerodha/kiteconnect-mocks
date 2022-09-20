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

let _TriggerRange;
let _Datum;
_Datum = PropTypes.shape({
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "lower": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "upper": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_TriggerRange = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.object, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const TriggerRange = _TriggerRange;
