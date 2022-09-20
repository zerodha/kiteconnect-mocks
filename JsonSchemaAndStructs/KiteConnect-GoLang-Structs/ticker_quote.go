// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    tickerQuote, err := UnmarshalTickerQuote(bytes)
//    bytes, err = tickerQuote.Marshal()

package TickerQuote

import "encoding/json"

type TickerQuote []TriggerRangeElement

func UnmarshalTickerQuote(data []byte) (TickerQuote, error) {
	var r TickerQuote
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TickerQuote) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type TriggerRangeElement struct {
	AverageTradedPrice *float64 `json:"average_traded_price,omitempty"`
	Change             *float64 `json:"change,omitempty"`              
	InstrumentToken    *int64   `json:"instrument_token,omitempty"`    
	LastPrice          *int64   `json:"last_price,omitempty"`          
	LastTradedQuantity *int64   `json:"last_traded_quantity,omitempty"`
	Mode               *string  `json:"mode,omitempty"`                
	Ohlc               *Ohlc    `json:"ohlc,omitempty"`                
	TotalBuyQuantity   *int64   `json:"total_buy_quantity,omitempty"`  
	TotalSellQuantity  *int64   `json:"total_sell_quantity,omitempty"` 
	Tradable           *bool    `json:"tradable,omitempty"`            
	VolumeTraded       *int64   `json:"volume_traded,omitempty"`       
}

type Ohlc struct {
	Close *int64 `json:"close,omitempty"`
	High  *int64 `json:"high,omitempty"` 
	Low   *int64 `json:"low,omitempty"`  
	Open  *int64 `json:"open,omitempty"` 
}
