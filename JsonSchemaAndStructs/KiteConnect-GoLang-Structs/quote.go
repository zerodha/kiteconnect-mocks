// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    quote, err := UnmarshalQuote(bytes)
//    bytes, err = quote.Marshal()

package main

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
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Buy     Buy          `json:"Buy"`    
	Data    Data         `json:"Data"`   
	Depth   Depth        `json:"Depth"`  
	NseInfy NseInfyClass `json:"NseInfy"`
	Ohlc    Ohlc         `json:"Ohlc"`   
	Quote   QuoteClass   `json:"Quote"`  
}

type Buy struct {
	AdditionalProperties bool          `json:"additionalProperties"`
	Properties           BuyProperties `json:"properties"`          
	Required             []string      `json:"required"`            
	Title                string        `json:"title"`               
	Type                 string        `json:"type"`                
}

type BuyProperties struct {
	Orders   Orders `json:"orders"`  
	Price    Orders `json:"price"`   
	Quantity Orders `json:"quantity"`
}

type Orders struct {
	Type Type `json:"type"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	NseInfy NseInfy `json:"NSE:INFY"`
}

type NseInfy struct {
	Ref string `json:"$ref"`
}

type Depth struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DepthProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DepthProperties struct {
	Buy  BuyClass `json:"buy"` 
	Sell BuyClass `json:"sell"`
}

type BuyClass struct {
	Items NseInfy `json:"items"`
	Type  string  `json:"type"` 
}

type NseInfyClass struct {
	AdditionalProperties bool              `json:"additionalProperties"`
	Properties           NseInfyProperties `json:"properties"`          
	Required             []string          `json:"required"`            
	Title                string            `json:"title"`               
	Type                 string            `json:"type"`                
}

type NseInfyProperties struct {
	AveragePrice      Orders        `json:"average_price"`      
	BuyQuantity       Orders        `json:"buy_quantity"`       
	Depth             NseInfy       `json:"depth"`              
	InstrumentToken   Orders        `json:"instrument_token"`   
	LastPrice         Orders        `json:"last_price"`         
	LastQuantity      Orders        `json:"last_quantity"`      
	LastTradeTime     LastTradeTime `json:"last_trade_time"`    
	LowerCircuitLimit Orders        `json:"lower_circuit_limit"`
	NetChange         Orders        `json:"net_change"`         
	Ohlc              NseInfy       `json:"ohlc"`               
	Oi                Orders        `json:"oi"`                 
	OiDayHigh         Orders        `json:"oi_day_high"`        
	OiDayLow          Orders        `json:"oi_day_low"`         
	SellQuantity      Orders        `json:"sell_quantity"`      
	Timestamp         LastTradeTime `json:"timestamp"`          
	UpperCircuitLimit Orders        `json:"upper_circuit_limit"`
	Volume            Orders        `json:"volume"`             
}

type LastTradeTime struct {
	Format string `json:"format"`
	Type   Type   `json:"type"`  
}

type Ohlc struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           OhlcProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type OhlcProperties struct {
	Close Orders `json:"close"`
	High  Orders `json:"high"` 
	Low   Orders `json:"low"`  
	Open  Orders `json:"open"` 
}

type QuoteClass struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           QuoteProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type QuoteProperties struct {
	Data   NseInfy `json:"data"`  
	Status Orders  `json:"status"`
}

type Type string
const (
	Integer Type = "integer"
	Number Type = "number"
	String Type = "string"
)
