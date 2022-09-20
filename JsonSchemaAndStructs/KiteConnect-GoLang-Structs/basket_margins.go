// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    basketMargins, err := UnmarshalBasketMargins(bytes)
//    bytes, err = basketMargins.Marshal()

package BasketMargins

import "encoding/json"

func UnmarshalBasketMargins(data []byte) (BasketMargins, error) {
	var r BasketMargins
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *BasketMargins) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type BasketMargins struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	Final   *Final  `json:"final,omitempty"`  
	Initial *Final  `json:"initial,omitempty"`
	Orders  []Final `json:"orders,omitempty"` 
}

type Final struct {
	Additional    *int64   `json:"additional,omitempty"`    
	Bo            *int64   `json:"bo,omitempty"`            
	Cash          *int64   `json:"cash,omitempty"`          
	Exchange      *string  `json:"exchange,omitempty"`      
	Exposure      *float64 `json:"exposure,omitempty"`      
	OptionPremium *float64 `json:"option_premium,omitempty"`
	Pnl           *Pnl     `json:"pnl,omitempty"`           
	Span          *float64 `json:"span,omitempty"`          
	Total         *float64 `json:"total,omitempty"`         
	Tradingsymbol *string  `json:"tradingsymbol,omitempty"` 
	Type          *string  `json:"type,omitempty"`          
	Var           *int64   `json:"var,omitempty"`           
}

type Pnl struct {
	Realised   *int64 `json:"realised,omitempty"`  
	Unrealised *int64 `json:"unrealised,omitempty"`
}
