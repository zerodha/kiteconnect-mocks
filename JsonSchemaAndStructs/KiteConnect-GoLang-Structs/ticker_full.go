// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    tickerFull, err := UnmarshalTickerFull(bytes)
//    bytes, err = tickerFull.Marshal()

package TickerFull

import "encoding/json"

type TickerFull []TriggerRangeElement

func UnmarshalTickerFull(data []byte) (TickerFull, error) {
	var r TickerFull
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TickerFull) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type TriggerRangeElement struct {
	AverageTradedPrice *float64 `json:"average_traded_price,omitempty"`
	Change             *float64 `json:"change,omitempty"`              
	Depth              *Depth   `json:"depth,omitempty"`               
	ExchangeTimestamp  *string  `json:"exchange_timestamp,omitempty"`  
	InstrumentToken    *int64   `json:"instrument_token,omitempty"`    
	LastPrice          *int64   `json:"last_price,omitempty"`          
	LastTradeTime      *string  `json:"last_trade_time,omitempty"`     
	LastTradedQuantity *int64   `json:"last_traded_quantity,omitempty"`
	Mode               *string  `json:"mode,omitempty"`                
	Ohlc               *Ohlc    `json:"ohlc,omitempty"`                
	Oi                 *int64   `json:"oi,omitempty"`                  
	OiDayHigh          *int64   `json:"oi_day_high,omitempty"`         
	OiDayLow           *int64   `json:"oi_day_low,omitempty"`          
	TotalBuyQuantity   *int64   `json:"total_buy_quantity,omitempty"`  
	TotalSellQuantity  *int64   `json:"total_sell_quantity,omitempty"` 
	Tradable           *bool    `json:"tradable,omitempty"`            
	VolumeTraded       *int64   `json:"volume_traded,omitempty"`       
}

type Depth struct {
	Buy  []Buy `json:"buy,omitempty"` 
	Sell []Buy `json:"sell,omitempty"`
}

type Buy struct {
	Orders   *int64 `json:"orders,omitempty"`  
	Price    *int64 `json:"price,omitempty"`   
	Quantity *int64 `json:"quantity,omitempty"`
}

type Ohlc struct {
	Close *int64 `json:"close,omitempty"`
	High  *int64 `json:"high,omitempty"` 
	Low   *int64 `json:"low,omitempty"`  
	Open  *int64 `json:"open,omitempty"` 
}
