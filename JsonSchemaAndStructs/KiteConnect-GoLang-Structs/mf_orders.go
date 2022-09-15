// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFOrders, err := UnmarshalMFOrders(bytes)
//    bytes, err = mFOrders.Marshal()

package main

import "encoding/json"

func UnmarshalMFOrders(data []byte) (MFOrders, error) {
	var r MFOrders
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFOrders) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFOrders struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum    Datum         `json:"Datum"`   
	MFOrders MFOrdersClass `json:"MFOrders"`
}

type Datum struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DatumProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DatumProperties struct {
	Amount            Amount          `json:"amount"`            
	AveragePrice      Amount          `json:"average_price"`     
	ExchangeOrderID   ExchangeOrderID `json:"exchange_order_id"` 
	ExchangeTimestamp ExchangeOrderID `json:"exchange_timestamp"`
	Folio             Amount          `json:"folio"`             
	Fund              Amount          `json:"fund"`              
	LastPrice         Amount          `json:"last_price"`        
	LastPriceDate     LastPriceDate   `json:"last_price_date"`   
	OrderID           LastPriceDate   `json:"order_id"`          
	OrderTimestamp    LastPriceDate   `json:"order_timestamp"`   
	PlacedBy          Amount          `json:"placed_by"`         
	PurchaseType      Amount          `json:"purchase_type"`     
	Quantity          Amount          `json:"quantity"`          
	SettlementID      ExchangeOrderID `json:"settlement_id"`     
	Status            Amount          `json:"status"`            
	StatusMessage     Amount          `json:"status_message"`    
	Tag               Tag             `json:"tag"`               
	Tradingsymbol     Amount          `json:"tradingsymbol"`     
	TransactionType   Amount          `json:"transaction_type"`  
	Variety           Amount          `json:"variety"`           
}

type Amount struct {
	Type Type `json:"type"`
}

type ExchangeOrderID struct {
	AnyOf []LastPriceDate `json:"anyOf"`
}

type LastPriceDate struct {
	Format *string `json:"format,omitempty"`
	Type   Type    `json:"type"`            
}

type Tag struct {
	AnyOf []Amount `json:"anyOf"`
}

type MFOrdersClass struct {
	AdditionalProperties bool               `json:"additionalProperties"`
	Properties           MFOrdersProperties `json:"properties"`          
	Required             []string           `json:"required"`            
	Title                string             `json:"title"`               
	Type                 string             `json:"type"`                
}

type MFOrdersProperties struct {
	Data   Data   `json:"data"`  
	Status Amount `json:"status"`
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
	Null Type = "null"
	Number Type = "number"
	String Type = "string"
)
