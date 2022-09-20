// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFInstruments, err := UnmarshalMFInstruments(bytes)
//    bytes, err = mFInstruments.Marshal()

package MfInstruments

import "encoding/json"

type MFInstruments []MFInstrument

func UnmarshalMFInstruments(data []byte) (MFInstruments, error) {
	var r MFInstruments
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFInstruments) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFInstrument struct {
	Amc                             *Amc            `json:"amc,omitempty"`                               
	DividendType                    *DividendType   `json:"dividend_type,omitempty"`                     
	LastPrice                       *float64        `json:"last_price,omitempty"`                        
	LastPriceDate                   *string         `json:"last_price_date,omitempty"`                   
	MinimumAdditionalPurchaseAmount *int64          `json:"minimum_additional_purchase_amount,omitempty"`
	MinimumPurchaseAmount           *int64          `json:"minimum_purchase_amount,omitempty"`           
	MinimumRedemptionQuantity       *float64        `json:"minimum_redemption_quantity,omitempty"`       
	Name                            *string         `json:"name,omitempty"`                              
	Plan                            *Plan           `json:"plan,omitempty"`                              
	PurchaseAllowed                 *int64          `json:"purchase_allowed,omitempty"`                  
	PurchaseAmountMultiplier        *int64          `json:"purchase_amount_multiplier,omitempty"`        
	RedemptionAllowed               *int64          `json:"redemption_allowed,omitempty"`                
	RedemptionQuantityMultiplier    *float64        `json:"redemption_quantity_multiplier,omitempty"`    
	SchemeType                      *SchemeType     `json:"scheme_type,omitempty"`                       
	SettlementType                  *SettlementType `json:"settlement_type,omitempty"`                   
	Tradingsymbol                   *string         `json:"tradingsymbol,omitempty"`                     
}

type Amc string
const (
	BirlaSunLifeMutualFundMF Amc = "BirlaSunLifeMutualFund_MF"
)

type DividendType string
const (
	Growth DividendType = "growth"
	Payout DividendType = "payout"
)

type Plan string
const (
	Direct Plan = "direct"
	Regular Plan = "regular"
)

type SchemeType string
const (
	Balanced SchemeType = "balanced"
	Debt SchemeType = "debt"
	Equity SchemeType = "equity"
	Fof SchemeType = "fof"
	Liquid SchemeType = "liquid"
)

type SettlementType string
const (
	T1 SettlementType = "T1"
	T3 SettlementType = "T3"
	T4 SettlementType = "T4"
	T6 SettlementType = "T6"
)
