// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    postback, err := UnmarshalPostback(bytes)
//    bytes, err = postback.Marshal()

package main

import "encoding/json"

func UnmarshalPostback(data []byte) (Postback, error) {
	var r Postback
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Postback) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Postback struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Meta     Meta          `json:"Meta"`    
	Postback PostbackClass `json:"Postback"`
}

type Meta struct {
	AdditionalProperties bool   `json:"additionalProperties"`
	Title                string `json:"title"`               
	Type                 string `json:"type"`                
}

type PostbackClass struct {
	AdditionalProperties bool       `json:"additionalProperties"`
	Properties           Properties `json:"properties"`          
	Required             []string   `json:"required"`            
	Title                string     `json:"title"`               
	Type                 string     `json:"type"`                
}

type Properties struct {
	AppID                   AppID     `json:"app_id"`                   
	AveragePrice            AppID     `json:"average_price"`            
	CancelledQuantity       AppID     `json:"cancelled_quantity"`       
	Checksum                AppID     `json:"checksum"`                 
	DisclosedQuantity       AppID     `json:"disclosed_quantity"`       
	Exchange                AppID     `json:"exchange"`                 
	ExchangeOrderID         AppID     `json:"exchange_order_id"`        
	ExchangeTimestamp       Timestamp `json:"exchange_timestamp"`       
	ExchangeUpdateTimestamp Timestamp `json:"exchange_update_timestamp"`
	FilledQuantity          AppID     `json:"filled_quantity"`          
	GUID                    AppID     `json:"guid"`                     
	InstrumentToken         AppID     `json:"instrument_token"`         
	MarketProtection        AppID     `json:"market_protection"`        
	Meta                    MetaClass `json:"meta"`                     
	OrderID                 AppID     `json:"order_id"`                 
	OrderTimestamp          Timestamp `json:"order_timestamp"`          
	OrderType               AppID     `json:"order_type"`               
	ParentOrderID           AppID     `json:"parent_order_id"`          
	PendingQuantity         AppID     `json:"pending_quantity"`         
	PlacedBy                AppID     `json:"placed_by"`                
	Price                   AppID     `json:"price"`                    
	Product                 AppID     `json:"product"`                  
	Quantity                AppID     `json:"quantity"`                 
	Status                  AppID     `json:"status"`                   
	StatusMessage           AppID     `json:"status_message"`           
	StatusMessageRaw        AppID     `json:"status_message_raw"`       
	Tag                     AppID     `json:"tag"`                      
	Tradingsymbol           AppID     `json:"tradingsymbol"`            
	TransactionType         AppID     `json:"transaction_type"`         
	TriggerPrice            AppID     `json:"trigger_price"`            
	UnfilledQuantity        AppID     `json:"unfilled_quantity"`        
	UserID                  AppID     `json:"user_id"`                  
	Validity                AppID     `json:"validity"`                 
	Variety                 AppID     `json:"variety"`                  
}

type AppID struct {
	Type Type `json:"type"`
}

type Timestamp struct {
	Format string `json:"format"`
	Type   Type   `json:"type"`  
}

type MetaClass struct {
	Ref string `json:"$ref"`
}

type Type string
const (
	Integer Type = "integer"
	Null Type = "null"
	String Type = "string"
)
