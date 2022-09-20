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

let _MFSips;
let _Datum;
_Datum = PropTypes.shape({
    "completed_instalments": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "created": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "dividend_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "frequency": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "fund": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instalment_amount": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "instalment_day": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "instalments": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_instalment": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "next_instalment": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "pending_instalments": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "sip_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "sip_reg_num": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "sip_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "step_up": PropTypes.oneOfType([PropTypes.object, PropTypes.any]),
    "tag": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "trigger_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_MFSips = PropTypes.shape({
    "data": PropTypes.oneOfType([PropTypes.arrayOf(_Datum), PropTypes.any]),
});

export const MFSips = _MFSips;
