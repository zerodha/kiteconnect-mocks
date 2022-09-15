// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orders, err := UnmarshalOrders(bytes)
//    bytes, err = orders.Marshal()

package main

import "encoding/json"

func UnmarshalOrders(data []byte) (Orders, error) {
	var r Orders
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Orders) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Orders struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum   Datum       `json:"Datum"`  
	Iceberg Iceberg     `json:"Iceberg"`
	Meta    MetaClass   `json:"Meta"`   
	Orders  OrdersClass `json:"Orders"` 
}

type Datum struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DatumProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DatumProperties struct {
	AveragePrice            AveragePrice       `json:"average_price"`            
	CancelledQuantity       AveragePrice       `json:"cancelled_quantity"`       
	DisclosedQuantity       AveragePrice       `json:"disclosed_quantity"`       
	Exchange                AveragePrice       `json:"exchange"`                 
	ExchangeOrderID         ExchangeOrderID    `json:"exchange_order_id"`        
	ExchangeTimestamp       ExchangeETimestamp `json:"exchange_timestamp"`       
	ExchangeUpdateTimestamp ExchangeETimestamp `json:"exchange_update_timestamp"`
	FilledQuantity          AveragePrice       `json:"filled_quantity"`          
	GUID                    AveragePrice       `json:"guid"`                     
	InstrumentToken         AveragePrice       `json:"instrument_token"`         
	MarketProtection        AveragePrice       `json:"market_protection"`        
	Meta                    Meta               `json:"meta"`                     
	Modified                AveragePrice       `json:"modified"`                 
	OrderID                 AveragePrice       `json:"order_id"`                 
	OrderTimestamp          OrderTimestamp     `json:"order_timestamp"`          
	OrderType               AveragePrice       `json:"order_type"`               
	ParentOrderID           AveragePrice       `json:"parent_order_id"`          
	PendingQuantity         AveragePrice       `json:"pending_quantity"`         
	PlacedBy                AveragePrice       `json:"placed_by"`                
	Price                   AveragePrice       `json:"price"`                    
	Product                 AveragePrice       `json:"product"`                  
	Quantity                AveragePrice       `json:"quantity"`                 
	Status                  AveragePrice       `json:"status"`                   
	StatusMessage           ExchangeOrderID    `json:"status_message"`           
	StatusMessageRaw        ExchangeOrderID    `json:"status_message_raw"`       
	Tag                     ExchangeOrderID    `json:"tag"`                      
	Tags                    Tags               `json:"tags"`                     
	Tradingsymbol           AveragePrice       `json:"tradingsymbol"`            
	TransactionType         AveragePrice       `json:"transaction_type"`         
	TriggerPrice            AveragePrice       `json:"trigger_price"`            
	Validity                AveragePrice       `json:"validity"`                 
	ValidityTTL             AveragePrice       `json:"validity_ttl"`             
	Variety                 AveragePrice       `json:"variety"`                  
}

type AveragePrice struct {
	Type Type `json:"type"`
}

type ExchangeOrderID struct {
	AnyOf []AveragePrice `json:"anyOf"`
}

type ExchangeETimestamp struct {
	AnyOf []OrderTimestamp `json:"anyOf"`
}

type OrderTimestamp struct {
	Format *string `json:"format,omitempty"`
	Type   Type    `json:"type"`            
}

type Meta struct {
	Ref string `json:"$ref"`
}

type Tags struct {
	Items AveragePrice `json:"items"`
	Type  string       `json:"type"` 
}

type Iceberg struct {
	AdditionalProperties bool              `json:"additionalProperties"`
	Properties           IcebergProperties `json:"properties"`          
	Required             []string          `json:"required"`            
	Title                string            `json:"title"`               
	Type                 string            `json:"type"`                
}

type IcebergProperties struct {
	Leg               AveragePrice `json:"leg"`               
	LegQuantity       AveragePrice `json:"leg_quantity"`      
	Legs              AveragePrice `json:"legs"`              
	RemainingQuantity AveragePrice `json:"remaining_quantity"`
	TotalQuantity     AveragePrice `json:"total_quantity"`    
}

type MetaClass struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           MetaProperties `json:"properties"`          
	Required             []interface{}  `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type MetaProperties struct {
	Iceberg Meta `json:"iceberg"`
}

type OrdersClass struct {
	AdditionalProperties bool             `json:"additionalProperties"`
	Properties           OrdersProperties `json:"properties"`          
	Required             []string         `json:"required"`            
	Title                string           `json:"title"`               
	Type                 string           `json:"type"`                
}

type OrdersProperties struct {
	Data   Data         `json:"data"`  
	Status AveragePrice `json:"status"`
}

type Data struct {
	Items Meta   `json:"items"`
	Type  string `json:"type"` 
}

type Type string
const (
	Boolean Type = "boolean"
	Integer Type = "integer"
	Null Type = "null"
	String Type = "string"
)
