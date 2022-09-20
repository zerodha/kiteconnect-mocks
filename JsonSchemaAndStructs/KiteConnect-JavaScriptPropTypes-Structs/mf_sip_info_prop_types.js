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

let _MFSIPInfo;
let _Data;
let _StepUp;
_StepUp = PropTypes.shape({
    "15-02": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_Data = PropTypes.shape({
    "completed_instalments": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "created": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "dividend_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "frequency": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "fund": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "fund_source": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instalment_amount": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "instalment_day": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "instalments": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_instalment": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "next_instalment": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "pending_instalments": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "sip_id": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "sip_reg_num": PropTypes.any,
    "sip_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "step_up": PropTypes.oneOfType([_StepUp, PropTypes.any]),
    "tag": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "transaction_type": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "trigger_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
});
_MFSIPInfo = PropTypes.shape({
    "data": PropTypes.oneOfType([_Data, PropTypes.any]),
    "status": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const MFSIPInfo = _MFSIPInfo;
