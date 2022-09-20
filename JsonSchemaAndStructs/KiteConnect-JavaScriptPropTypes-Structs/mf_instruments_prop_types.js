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

let _MFInstruments;
const _Amc = PropTypes.oneOfType(['birlaSunLifeMutualFundMF']);
const _DividendType = PropTypes.oneOfType(['growth', 'payout']);
const _Plan = PropTypes.oneOfType(['direct', 'regular']);
const _SchemeType = PropTypes.oneOfType(['balanced', 'debt', 'equity', 'fof', 'liquid']);
const _SettlementType = PropTypes.oneOfType(['t1', 't3', 't4', 't6']);
_MFInstruments = PropTypes.shape({
    "amc": PropTypes.oneOfType([_Amc, PropTypes.any]),
    "dividend_type": PropTypes.oneOfType([_DividendType, PropTypes.any]),
    "last_price": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "last_price_date": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "minimum_additional_purchase_amount": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "minimum_purchase_amount": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "minimum_redemption_quantity": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "name": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
    "plan": PropTypes.oneOfType([_Plan, PropTypes.any]),
    "purchase_allowed": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "purchase_amount_multiplier": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "redemption_allowed": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "redemption_quantity_multiplier": PropTypes.oneOfType([PropTypes.number, PropTypes.any]),
    "scheme_type": PropTypes.oneOfType([_SchemeType, PropTypes.any]),
    "settlement_type": PropTypes.oneOfType([_SettlementType, PropTypes.any]),
    "tradingsymbol": PropTypes.oneOfType([PropTypes.any, PropTypes.string]),
});

export const MFInstruments = PropTypes.arrayOf(_MFInstruments);
