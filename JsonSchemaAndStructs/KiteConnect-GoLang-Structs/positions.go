// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    positions, err := UnmarshalPositions(bytes)
//    bytes, err = positions.Marshal()

package Positions

import "encoding/json"

func UnmarshalPositions(data []byte) (Positions, error) {
	var r Positions
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Positions) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Positions struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	Day []Day `json:"day,omitempty"`
	Net []Day `json:"net,omitempty"`
}

type Day struct {
	AveragePrice      *float64 `json:"average_price,omitempty"`     
	BuyM2M            *int64   `json:"buy_m2m,omitempty"`           
	BuyPrice          *float64 `json:"buy_price,omitempty"`         
	BuyQuantity       *int64   `json:"buy_quantity,omitempty"`      
	BuyValue          *int64   `json:"buy_value,omitempty"`         
	ClosePrice        *int64   `json:"close_price,omitempty"`       
	DayBuyPrice       *float64 `json:"day_buy_price,omitempty"`     
	DayBuyQuantity    *int64   `json:"day_buy_quantity,omitempty"`  
	DayBuyValue       *int64   `json:"day_buy_value,omitempty"`     
	DaySellPrice      *int64   `json:"day_sell_price,omitempty"`    
	DaySellQuantity   *int64   `json:"day_sell_quantity,omitempty"` 
	DaySellValue      *int64   `json:"day_sell_value,omitempty"`    
	Exchange          *string  `json:"exchange,omitempty"`          
	InstrumentToken   *int64   `json:"instrument_token,omitempty"`  
	LastPrice         *float64 `json:"last_price,omitempty"`        
	M2M               *int64   `json:"m2m,omitempty"`               
	Multiplier        *int64   `json:"multiplier,omitempty"`        
	OvernightQuantity *int64   `json:"overnight_quantity,omitempty"`
	Pnl               *int64   `json:"pnl,omitempty"`               
	Product           *string  `json:"product,omitempty"`           
	Quantity          *int64   `json:"quantity,omitempty"`          
	Realised          *int64   `json:"realised,omitempty"`          
	SellM2M           *int64   `json:"sell_m2m,omitempty"`          
	SellPrice         *int64   `json:"sell_price,omitempty"`        
	SellQuantity      *int64   `json:"sell_quantity,omitempty"`     
	SellValue         *int64   `json:"sell_value,omitempty"`        
	Tradingsymbol     *string  `json:"tradingsymbol,omitempty"`     
	Unrealised        *int64   `json:"unrealised,omitempty"`        
	Value             *int64   `json:"value,omitempty"`             
}
