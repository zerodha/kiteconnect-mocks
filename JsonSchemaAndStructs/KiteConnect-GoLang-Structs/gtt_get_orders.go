// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    gttGetOrders, err := UnmarshalGttGetOrders(bytes)
//    bytes, err = gttGetOrders.Marshal()

package GttGetOrders

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
	Data   []Datum `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Datum struct {
	Condition     *Condition  `json:"condition,omitempty"` 
	CreatedAt     *string     `json:"created_at,omitempty"`
	ExpiresAt     *string     `json:"expires_at,omitempty"`
	ID            *int64      `json:"id,omitempty"`        
	Meta          *Meta       `json:"meta"`                
	Orders        []Order     `json:"orders,omitempty"`    
	ParentTrigger interface{} `json:"parent_trigger"`      
	Status        *string     `json:"status,omitempty"`    
	Type          *string     `json:"type,omitempty"`      
	UpdatedAt     *string     `json:"updated_at,omitempty"`
	UserID        *string     `json:"user_id,omitempty"`   
}

type Condition struct {
	Exchange        *string   `json:"exchange,omitempty"`        
	InstrumentToken *int64    `json:"instrument_token,omitempty"`
	LastPrice       *float64  `json:"last_price,omitempty"`      
	Tradingsymbol   *string   `json:"tradingsymbol,omitempty"`   
	TriggerValues   []float64 `json:"trigger_values,omitempty"`  
}

type Meta struct {
}

type Order struct {
	Exchange        *string  `json:"exchange,omitempty"`        
	OrderType       *string  `json:"order_type,omitempty"`      
	Price           *float64 `json:"price,omitempty"`           
	Product         *string  `json:"product,omitempty"`         
	Quantity        *int64   `json:"quantity,omitempty"`        
	Result          *Result  `json:"result"`                    
	Tradingsymbol   *string  `json:"tradingsymbol,omitempty"`   
	TransactionType *string  `json:"transaction_type,omitempty"`
}

type Result struct {
	AccountID       *string      `json:"account_id,omitempty"`      
	Exchange        *string      `json:"exchange,omitempty"`        
	Meta            *string      `json:"meta,omitempty"`            
	OrderResult     *OrderResult `json:"order_result,omitempty"`    
	OrderType       *string      `json:"order_type,omitempty"`      
	Price           *int64       `json:"price,omitempty"`           
	Product         *string      `json:"product,omitempty"`         
	Quantity        *int64       `json:"quantity,omitempty"`        
	Timestamp       *string      `json:"timestamp,omitempty"`       
	Tradingsymbol   *string      `json:"tradingsymbol,omitempty"`   
	TransactionType *string      `json:"transaction_type,omitempty"`
	TriggeredAt     *float64     `json:"triggered_at,omitempty"`    
	Validity        *string      `json:"validity,omitempty"`        
}

type OrderResult struct {
	OrderID         *string `json:"order_id,omitempty"`        
	RejectionReason *string `json:"rejection_reason,omitempty"`
	Status          *string `json:"status,omitempty"`          
}
