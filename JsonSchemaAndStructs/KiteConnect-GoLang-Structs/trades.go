// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    trades, err := UnmarshalTrades(bytes)
//    bytes, err = trades.Marshal()

package main

import "encoding/json"

func UnmarshalTrades(data []byte) (Trades, error) {
	var r Trades
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Trades) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Trades struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum  Datum       `json:"Datum"` 
	Trades TradesClass `json:"Trades"`
}

type Datum struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DatumProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DatumProperties struct {
	AveragePrice      AveragePrice      `json:"average_price"`     
	Exchange          AveragePrice      `json:"exchange"`          
	ExchangeOrderID   AveragePrice      `json:"exchange_order_id"` 
	ExchangeTimestamp ExchangeTimestamp `json:"exchange_timestamp"`
	FillTimestamp     ExchangeTimestamp `json:"fill_timestamp"`    
	InstrumentToken   AveragePrice      `json:"instrument_token"`  
	OrderID           AveragePrice      `json:"order_id"`          
	OrderTimestamp    ExchangeTimestamp `json:"order_timestamp"`   
	Product           AveragePrice      `json:"product"`           
	Quantity          AveragePrice      `json:"quantity"`          
	TradeID           ExchangeTimestamp `json:"trade_id"`          
	Tradingsymbol     AveragePrice      `json:"tradingsymbol"`     
	TransactionType   AveragePrice      `json:"transaction_type"`  
}

type AveragePrice struct {
	Type Type `json:"type"`
}

type ExchangeTimestamp struct {
	Format string `json:"format"`
	Type   Type   `json:"type"`  
}

type TradesClass struct {
	AdditionalProperties bool             `json:"additionalProperties"`
	Properties           TradesProperties `json:"properties"`          
	Required             []string         `json:"required"`            
	Title                string           `json:"title"`               
	Type                 string           `json:"type"`                
}

type TradesProperties struct {
	Data   Data         `json:"data"`  
	Status AveragePrice `json:"status"`
}

type Data struct {
	Items Items  `json:"items"`
	Type  string `json:"type"` 
}

type Items struct {
	Ref string `json:"$ref"`
}

type Type string
const (
	Integer Type = "integer"
	Number Type = "number"
	String Type = "string"
)
