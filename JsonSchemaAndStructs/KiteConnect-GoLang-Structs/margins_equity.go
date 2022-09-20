// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    marginsEquity, err := UnmarshalMarginsEquity(bytes)
//    bytes, err = marginsEquity.Marshal()

package  MarginsEquity

import "encoding/json"

func UnmarshalMarginsEquity(data []byte) (MarginsEquity, error) {
	var r MarginsEquity
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MarginsEquity) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MarginsEquity struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	Available *Available         `json:"available,omitempty"`
	Enabled   *bool              `json:"enabled,omitempty"`  
	Net       *float64           `json:"net,omitempty"`      
	Utilised  map[string]float64 `json:"utilised,omitempty"` 
}

type Available struct {
	AdhocMargin    *int64   `json:"adhoc_margin,omitempty"`   
	Cash           *float64 `json:"cash,omitempty"`           
	Collateral     *int64   `json:"collateral,omitempty"`     
	IntradayPayin  *int64   `json:"intraday_payin,omitempty"` 
	LiveBalance    *float64 `json:"live_balance,omitempty"`   
	OpeningBalance *float64 `json:"opening_balance,omitempty"`
}
