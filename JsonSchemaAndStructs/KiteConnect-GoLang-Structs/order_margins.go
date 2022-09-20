// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderMargins, err := UnmarshalOrderMargins(bytes)
//    bytes, err = orderMargins.Marshal()

package OrderMargins

import "encoding/json"

func UnmarshalOrderMargins(data []byte) (OrderMargins, error) {
	var r OrderMargins
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderMargins) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderMargins struct {
	Data   []Datum `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Datum struct {
	Additional    *int64   `json:"additional,omitempty"`    
	Bo            *int64   `json:"bo,omitempty"`            
	Cash          *int64   `json:"cash,omitempty"`          
	Exchange      *string  `json:"exchange,omitempty"`      
	Exposure      *int64   `json:"exposure,omitempty"`      
	OptionPremium *int64   `json:"option_premium,omitempty"`
	Pnl           *Pnl     `json:"pnl,omitempty"`           
	Span          *int64   `json:"span,omitempty"`          
	Total         *float64 `json:"total,omitempty"`         
	Tradingsymbol *string  `json:"tradingsymbol,omitempty"` 
	Type          *string  `json:"type,omitempty"`          
	Var           *float64 `json:"var,omitempty"`           
}

type Pnl struct {
	Realised   *int64 `json:"realised,omitempty"`  
	Unrealised *int64 `json:"unrealised,omitempty"`
}
