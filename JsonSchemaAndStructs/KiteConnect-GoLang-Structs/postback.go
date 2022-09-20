// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    postback, err := UnmarshalPostback(bytes)
//    bytes, err = postback.Marshal()

package Postback

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
	AppID                   *int64      `json:"app_id,omitempty"`                   
	AveragePrice            *int64      `json:"average_price,omitempty"`            
	CancelledQuantity       *int64      `json:"cancelled_quantity,omitempty"`       
	Checksum                *string     `json:"checksum,omitempty"`                 
	DisclosedQuantity       *int64      `json:"disclosed_quantity,omitempty"`       
	Exchange                *string     `json:"exchange,omitempty"`                 
	ExchangeOrderID         *string     `json:"exchange_order_id,omitempty"`        
	ExchangeTimestamp       *string     `json:"exchange_timestamp,omitempty"`       
	ExchangeUpdateTimestamp *string     `json:"exchange_update_timestamp,omitempty"`
	FilledQuantity          *int64      `json:"filled_quantity,omitempty"`          
	GUID                    *string     `json:"guid,omitempty"`                     
	InstrumentToken         *int64      `json:"instrument_token,omitempty"`         
	MarketProtection        *int64      `json:"market_protection,omitempty"`        
	Meta                    *Meta       `json:"meta,omitempty"`                     
	OrderID                 *string     `json:"order_id,omitempty"`                 
	OrderTimestamp          *string     `json:"order_timestamp,omitempty"`          
	OrderType               *string     `json:"order_type,omitempty"`               
	ParentOrderID           interface{} `json:"parent_order_id"`                    
	PendingQuantity         *int64      `json:"pending_quantity,omitempty"`         
	PlacedBy                *string     `json:"placed_by,omitempty"`                
	Price                   *int64      `json:"price,omitempty"`                    
	Product                 *string     `json:"product,omitempty"`                  
	Quantity                *int64      `json:"quantity,omitempty"`                 
	Status                  *string     `json:"status,omitempty"`                   
	StatusMessage           interface{} `json:"status_message"`                     
	StatusMessageRaw        interface{} `json:"status_message_raw"`                 
	Tag                     interface{} `json:"tag"`                                
	Tradingsymbol           *string     `json:"tradingsymbol,omitempty"`            
	TransactionType         *string     `json:"transaction_type,omitempty"`         
	TriggerPrice            *int64      `json:"trigger_price,omitempty"`            
	UnfilledQuantity        *int64      `json:"unfilled_quantity,omitempty"`        
	UserID                  *string     `json:"user_id,omitempty"`                  
	Validity                *string     `json:"validity,omitempty"`                 
	Variety                 *string     `json:"variety,omitempty"`                  
}

type Meta struct {
}
