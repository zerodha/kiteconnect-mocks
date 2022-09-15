// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFOrdersInfo, err := UnmarshalMFOrdersInfo(bytes)
//    bytes, err = mFOrdersInfo.Marshal()

package main

import "encoding/json"

func UnmarshalMFOrdersInfo(data []byte) (MFOrdersInfo, error) {
	var r MFOrdersInfo
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFOrdersInfo) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFOrdersInfo struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data         Data              `json:"Data"`        
	MFOrdersInfo MFOrdersInfoClass `json:"MFOrdersInfo"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	Amount            Amount        `json:"amount"`            
	AveragePrice      Amount        `json:"average_price"`     
	ExchangeOrderID   Amount        `json:"exchange_order_id"` 
	ExchangeTimestamp Amount        `json:"exchange_timestamp"`
	Folio             Amount        `json:"folio"`             
	Fund              Amount        `json:"fund"`              
	LastPrice         Amount        `json:"last_price"`        
	LastPriceDate     LastPriceDate `json:"last_price_date"`   
	OrderID           LastPriceDate `json:"order_id"`          
	OrderTimestamp    LastPriceDate `json:"order_timestamp"`   
	PlacedBy          Amount        `json:"placed_by"`         
	PurchaseType      Amount        `json:"purchase_type"`     
	Quantity          Amount        `json:"quantity"`          
	SettlementID      Amount        `json:"settlement_id"`     
	Status            Amount        `json:"status"`            
	StatusMessage     Amount        `json:"status_message"`    
	Tag               Amount        `json:"tag"`               
	Tradingsymbol     Amount        `json:"tradingsymbol"`     
	TransactionType   Amount        `json:"transaction_type"`  
	Variety           Amount        `json:"variety"`           
}

type Amount struct {
	Type Type `json:"type"`
}

type LastPriceDate struct {
	Format string `json:"format"`
	Type   Type   `json:"type"`  
}

type MFOrdersInfoClass struct {
	AdditionalProperties bool                   `json:"additionalProperties"`
	Properties           MFOrdersInfoProperties `json:"properties"`          
	Required             []string               `json:"required"`            
	Title                string                 `json:"title"`               
	Type                 string                 `json:"type"`                
}

type MFOrdersInfoProperties struct {
	Data   DataClass `json:"data"`  
	Status Amount    `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}

type Type string
const (
	Integer Type = "integer"
	Null Type = "null"
	Number Type = "number"
	String Type = "string"
)
