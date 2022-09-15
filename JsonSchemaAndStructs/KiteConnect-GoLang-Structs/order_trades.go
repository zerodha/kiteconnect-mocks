// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderTrades, err := UnmarshalOrderTrades(bytes)
//    bytes, err = orderTrades.Marshal()

package main

import "encoding/json"

func UnmarshalOrderTrades(data []byte) (OrderTrades, error) {
	var r OrderTrades
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderTrades) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderTrades struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum       Datum            `json:"Datum"`      
	OrderTrades OrderTradesClass `json:"OrderTrades"`
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

type OrderTradesClass struct {
	AdditionalProperties bool                  `json:"additionalProperties"`
	Properties           OrderTradesProperties `json:"properties"`          
	Required             []string              `json:"required"`            
	Title                string                `json:"title"`               
	Type                 string                `json:"type"`                
}

type OrderTradesProperties struct {
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
	String Type = "string"
)
