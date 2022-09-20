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

let _InstrumentsNse;
const _Exchange = PropTypes.oneOfType(['nse']);
const _InstrumentType = PropTypes.oneOfType(['eq']);
_InstrumentsNse = PropTypes.shape({
    "exchange": PropTypes.oneOfType([_Exchange, PropTypes.any]),
    "exchange_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "expiry": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "instrument_type": PropTypes.oneOfType([_InstrumentType, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "lot_size": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "name": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "segment": PropTypes.oneOfType([_Exchange, PropTypes.any]),
    "strike": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tick_size": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const InstrumentsNse = PropTypes.arrayOf(_InstrumentsNse);
