// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderInfo, err := UnmarshalOrderInfo(bytes)
//    bytes, err = orderInfo.Marshal()

package main

import "encoding/json"

func UnmarshalOrderInfo(data []byte) (OrderInfo, error) {
	var r OrderInfo
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderInfo) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderInfo struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum     Datum          `json:"Datum"`    
	OrderInfo OrderInfoClass `json:"OrderInfo"`
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
	CancelledQuantity AveragePrice      `json:"cancelled_quantity"`
	DisclosedQuantity AveragePrice      `json:"disclosed_quantity"`
	Exchange          AveragePrice      `json:"exchange"`          
	ExchangeOrderID   ExchangeOrderID   `json:"exchange_order_id"` 
	ExchangeTimestamp ExchangeTimestamp `json:"exchange_timestamp"`
	FilledQuantity    AveragePrice      `json:"filled_quantity"`   
	InstrumentToken   AveragePrice      `json:"instrument_token"`  
	OrderID           AveragePrice      `json:"order_id"`          
	OrderTimestamp    OrderTimestamp    `json:"order_timestamp"`   
	OrderType         AveragePrice      `json:"order_type"`        
	ParentOrderID     AveragePrice      `json:"parent_order_id"`   
	PendingQuantity   AveragePrice      `json:"pending_quantity"`  
	PlacedBy          AveragePrice      `json:"placed_by"`         
	Price             AveragePrice      `json:"price"`             
	Product           AveragePrice      `json:"product"`           
	Quantity          AveragePrice      `json:"quantity"`          
	Status            AveragePrice      `json:"status"`            
	StatusMessage     AveragePrice      `json:"status_message"`    
	Tag               AveragePrice      `json:"tag"`               
	Tradingsymbol     AveragePrice      `json:"tradingsymbol"`     
	TransactionType   AveragePrice      `json:"transaction_type"`  
	TriggerPrice      AveragePrice      `json:"trigger_price"`     
	Validity          AveragePrice      `json:"validity"`          
	Variety           AveragePrice      `json:"variety"`           
}

type AveragePrice struct {
	Type Type `json:"type"`
}

type ExchangeOrderID struct {
	AnyOf []AveragePrice `json:"anyOf"`
}

type ExchangeTimestamp struct {
	AnyOf []OrderTimestamp `json:"anyOf"`
}

type OrderTimestamp struct {
	Format *string `json:"format,omitempty"`
	Type   Type    `json:"type"`            
}

type OrderInfoClass struct {
	AdditionalProperties bool                `json:"additionalProperties"`
	Properties           OrderInfoProperties `json:"properties"`          
	Required             []string            `json:"required"`            
	Title                string              `json:"title"`               
	Type                 string              `json:"type"`                
}

type OrderInfoProperties struct {
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
	Null Type = "null"
	Number Type = "number"
	String Type = "string"
)
