// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    holdings, err := UnmarshalHoldings(bytes)
//    bytes, err = holdings.Marshal()

package Holdings

import "encoding/json"

func UnmarshalHoldings(data []byte) (Holdings, error) {
	var r Holdings
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Holdings) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Holdings struct {
	Data   []Datum `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Datum struct {
	AuthorisedDate      *string  `json:"authorised_date,omitempty"`      
	AuthorisedQuantity  *int64   `json:"authorised_quantity,omitempty"`  
	AveragePrice        *float64 `json:"average_price,omitempty"`        
	ClosePrice          *float64 `json:"close_price,omitempty"`          
	CollateralQuantity  *int64   `json:"collateral_quantity,omitempty"`  
	CollateralType      *string  `json:"collateral_type,omitempty"`      
	DayChange           *float64 `json:"day_change,omitempty"`           
	DayChangePercentage *float64 `json:"day_change_percentage,omitempty"`
	Discrepancy         *bool    `json:"discrepancy,omitempty"`          
	Exchange            *string  `json:"exchange,omitempty"`             
	InstrumentToken     *int64   `json:"instrument_token,omitempty"`     
	Isin                *string  `json:"isin,omitempty"`                 
	LastPrice           *float64 `json:"last_price,omitempty"`           
	OpeningQuantity     *int64   `json:"opening_quantity,omitempty"`     
	Pnl                 *float64 `json:"pnl,omitempty"`                  
	Price               *int64   `json:"price,omitempty"`                
	Product             *string  `json:"product,omitempty"`              
	Quantity            *int64   `json:"quantity,omitempty"`             
	RealisedQuantity    *int64   `json:"realised_quantity,omitempty"`    
	T1Quantity          *int64   `json:"t1_quantity,omitempty"`          
	Tradingsymbol       *string  `json:"tradingsymbol,omitempty"`        
	UsedQuantity        *int64   `json:"used_quantity,omitempty"`        
}
