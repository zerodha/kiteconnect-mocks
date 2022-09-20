// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    margins, err := UnmarshalMargins(bytes)
//    bytes, err = margins.Marshal()

package Margins

import "encoding/json"

func UnmarshalMargins(data []byte) (Margins, error) {
	var r Margins
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Margins) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Margins struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	Commodity *Ity `json:"commodity,omitempty"`
	Equity    *Ity `json:"equity,omitempty"`   
}

type Ity struct {
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
