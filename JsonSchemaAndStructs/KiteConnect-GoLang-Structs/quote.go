// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    quote, err := UnmarshalQuote(bytes)
//    bytes, err = quote.Marshal()

package Quote

import "encoding/json"

func UnmarshalQuote(data []byte) (Quote, error) {
	var r Quote
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Quote) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Quote struct {
	Data   map[string]Datum `json:"data,omitempty"`  
	Status *string          `json:"status,omitempty"`
}

type Datum struct {
	AveragePrice      *float64 `json:"average_price,omitempty"`      
	BuyQuantity       *int64   `json:"buy_quantity,omitempty"`       
	Depth             *Depth   `json:"depth,omitempty"`              
	InstrumentToken   *int64   `json:"instrument_token,omitempty"`   
	LastPrice         *float64 `json:"last_price,omitempty"`         
	LastQuantity      *int64   `json:"last_quantity,omitempty"`      
	LastTradeTime     *string  `json:"last_trade_time,omitempty"`    
	LowerCircuitLimit *float64 `json:"lower_circuit_limit,omitempty"`
	NetChange         *int64   `json:"net_change,omitempty"`         
	Ohlc              *Ohlc    `json:"ohlc,omitempty"`               
	Oi                *int64   `json:"oi,omitempty"`                 
	OiDayHigh         *int64   `json:"oi_day_high,omitempty"`        
	OiDayLow          *int64   `json:"oi_day_low,omitempty"`         
	SellQuantity      *int64   `json:"sell_quantity,omitempty"`      
	Timestamp         *string  `json:"timestamp,omitempty"`          
	UpperCircuitLimit *float64 `json:"upper_circuit_limit,omitempty"`
	Volume            *int64   `json:"volume,omitempty"`             
}

type Depth struct {
	Buy  []Buy `json:"buy,omitempty"` 
	Sell []Buy `json:"sell,omitempty"`
}

type Buy struct {
	Orders   *int64   `json:"orders,omitempty"`  
	Price    *float64 `json:"price,omitempty"`   
	Quantity *int64   `json:"quantity,omitempty"`
}

type Ohlc struct {
	Close *float64 `json:"close,omitempty"`
	High  *float64 `json:"high,omitempty"` 
	Low   *float64 `json:"low,omitempty"`  
	Open  *int64   `json:"open,omitempty"` 
}
