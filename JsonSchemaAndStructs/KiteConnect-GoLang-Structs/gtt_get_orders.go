// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    gttGetOrders, err := UnmarshalGttGetOrders(bytes)
//    bytes, err = gttGetOrders.Marshal()

package main

import "encoding/json"

func UnmarshalGttGetOrders(data []byte) (GttGetOrders, error) {
	var r GttGetOrders
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GttGetOrders) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GttGetOrders struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Condition    Condition         `json:"Condition"`   
	Datum        Datum             `json:"Datum"`       
	GttGetOrders GttGetOrdersClass `json:"GttGetOrders"`
	Meta         MetaClass         `json:"Meta"`        
	Order        Order             `json:"Order"`       
	OrderResult  OrderResult       `json:"OrderResult"` 
	Result       Result            `json:"Result"`      
}

type Condition struct {
	AdditionalProperties bool                `json:"additionalProperties"`
	Properties           ConditionProperties `json:"properties"`          
	Required             []string            `json:"required"`            
	Title                string              `json:"title"`               
	Type                 string              `json:"type"`                
}

type ConditionProperties struct {
	Exchange        Exchange      `json:"exchange"`        
	InstrumentToken Exchange      `json:"instrument_token"`
	LastPrice       Exchange      `json:"last_price"`      
	Tradingsymbol   Exchange      `json:"tradingsymbol"`   
	TriggerValues   TriggerValues `json:"trigger_values"`  
}

type Exchange struct {
	Type Type `json:"type"`
}

type TriggerValues struct {
	Items Exchange `json:"items"`
	Type  string   `json:"type"` 
}

type Datum struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DatumProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DatumProperties struct {
	Condition     ConditionClass `json:"condition"`     
	CreatedAt     CreatedAt      `json:"created_at"`    
	ExpiresAt     CreatedAt      `json:"expires_at"`    
	ID            Exchange       `json:"id"`            
	Meta          Meta           `json:"meta"`          
	Orders        Orders         `json:"orders"`        
	ParentTrigger Exchange       `json:"parent_trigger"`
	Status        Exchange       `json:"status"`        
	Type          Exchange       `json:"type"`          
	UpdatedAt     CreatedAt      `json:"updated_at"`    
	UserID        Exchange       `json:"user_id"`       
}

type ConditionClass struct {
	Ref string `json:"$ref"`
}

type CreatedAt struct {
	Format string `json:"format"`
	Type   Type   `json:"type"`  
}

type Meta struct {
	AnyOf []AnyOf `json:"anyOf"`
}

type AnyOf struct {
	Ref  *string `json:"$ref,omitempty"`
	Type *Type   `json:"type,omitempty"`
}

type Orders struct {
	Items ConditionClass `json:"items"`
	Type  string         `json:"type"` 
}

type GttGetOrdersClass struct {
	AdditionalProperties bool                   `json:"additionalProperties"`
	Properties           GttGetOrdersProperties `json:"properties"`          
	Required             []string               `json:"required"`            
	Title                string                 `json:"title"`               
	Type                 string                 `json:"type"`                
}

type GttGetOrdersProperties struct {
	Data   Orders   `json:"data"`  
	Status Exchange `json:"status"`
}

type MetaClass struct {
	AdditionalProperties bool   `json:"additionalProperties"`
	Title                string `json:"title"`               
	Type                 string `json:"type"`                
}

type Order struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           OrderProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type OrderProperties struct {
	Exchange        Exchange `json:"exchange"`        
	OrderType       Exchange `json:"order_type"`      
	Price           Exchange `json:"price"`           
	Product         Exchange `json:"product"`         
	Quantity        Exchange `json:"quantity"`        
	Result          Meta     `json:"result"`          
	Tradingsymbol   Exchange `json:"tradingsymbol"`   
	TransactionType Exchange `json:"transaction_type"`
}

type OrderResult struct {
	AdditionalProperties bool                  `json:"additionalProperties"`
	Properties           OrderResultProperties `json:"properties"`          
	Required             []string              `json:"required"`            
	Title                string                `json:"title"`               
	Type                 string                `json:"type"`                
}

type OrderResultProperties struct {
	OrderID         Exchange `json:"order_id"`        
	RejectionReason Exchange `json:"rejection_reason"`
	Status          Exchange `json:"status"`          
}

type Result struct {
	AdditionalProperties bool             `json:"additionalProperties"`
	Properties           ResultProperties `json:"properties"`          
	Required             []string         `json:"required"`            
	Title                string           `json:"title"`               
	Type                 string           `json:"type"`                
}

type ResultProperties struct {
	AccountID       Exchange       `json:"account_id"`      
	Exchange        Exchange       `json:"exchange"`        
	Meta            Exchange       `json:"meta"`            
	OrderResult     ConditionClass `json:"order_result"`    
	OrderType       Exchange       `json:"order_type"`      
	Price           Exchange       `json:"price"`           
	Product         Exchange       `json:"product"`         
	Quantity        Exchange       `json:"quantity"`        
	Timestamp       CreatedAt      `json:"timestamp"`       
	Tradingsymbol   Exchange       `json:"tradingsymbol"`   
	TransactionType Exchange       `json:"transaction_type"`
	TriggeredAt     Exchange       `json:"triggered_at"`    
	Validity        Exchange       `json:"validity"`        
}

type Type string
const (
	Integer Type = "integer"
	Null Type = "null"
	Number Type = "number"
	String Type = "string"
)
