// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFHoldings, err := UnmarshalMFHoldings(bytes)
//    bytes, err = mFHoldings.Marshal()

package MfHoldings

import "encoding/json"

func UnmarshalMFHoldings(data []byte) (MFHoldings, error) {
	var r MFHoldings
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFHoldings) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFHoldings struct {
	Data   []Datum `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Datum struct {
	AveragePrice    *float64 `json:"average_price,omitempty"`   
	Folio           *string  `json:"folio,omitempty"`           
	Fund            *string  `json:"fund,omitempty"`            
	LastPrice       *float64 `json:"last_price,omitempty"`      
	LastPriceDate   *string  `json:"last_price_date,omitempty"` 
	PledgedQuantity *int64   `json:"pledged_quantity,omitempty"`
	Pnl             *int64   `json:"pnl,omitempty"`             
	Quantity        *float64 `json:"quantity,omitempty"`        
	Tradingsymbol   *string  `json:"tradingsymbol,omitempty"`   
}
