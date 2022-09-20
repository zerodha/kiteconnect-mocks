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

let _InstrumentsAll;
const _Exchange = PropTypes.oneOfType(['bse', 'nfo', 'nse']);
const _InstrumentType = PropTypes.oneOfType(['ce', 'eq', 'pe']);
const _Segment = PropTypes.oneOfType(['bse', 'nfoOpt', 'nse']);
_InstrumentsAll = PropTypes.shape({
    "exchange": PropTypes.oneOfType([_Exchange, PropTypes.any]),
    "exchange_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "expiry": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "instrument_token": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "instrument_type": PropTypes.oneOfType([_InstrumentType, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "lot_size": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "name": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "segment": PropTypes.oneOfType([_Segment, PropTypes.any]),
    "strike": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tick_size": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const InstrumentsAll = PropTypes.arrayOf(_InstrumentsAll);
