// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orders, err := UnmarshalOrders(bytes)
//    bytes, err = orders.Marshal()

package Orders

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
	Data   []Datum `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Datum struct {
	AveragePrice            *int64      `json:"average_price,omitempty"`     
	CancelledQuantity       *int64      `json:"cancelled_quantity,omitempty"`
	DisclosedQuantity       *int64      `json:"disclosed_quantity,omitempty"`
	Exchange                *string     `json:"exchange,omitempty"`          
	ExchangeOrderID         *string     `json:"exchange_order_id"`           
	ExchangeTimestamp       *string     `json:"exchange_timestamp"`          
	ExchangeUpdateTimestamp *string     `json:"exchange_update_timestamp"`   
	FilledQuantity          *int64      `json:"filled_quantity,omitempty"`   
	GUID                    *string     `json:"guid,omitempty"`              
	InstrumentToken         *int64      `json:"instrument_token,omitempty"`  
	MarketProtection        *int64      `json:"market_protection,omitempty"` 
	Meta                    *Meta       `json:"meta,omitempty"`              
	Modified                *bool       `json:"modified,omitempty"`          
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
	StatusMessage           *string     `json:"status_message"`              
	StatusMessageRaw        *string     `json:"status_message_raw"`          
	Tag                     *string     `json:"tag"`                         
	Tags                    []string    `json:"tags,omitempty"`              
	Tradingsymbol           *string     `json:"tradingsymbol,omitempty"`     
	TransactionType         *string     `json:"transaction_type,omitempty"`  
	TriggerPrice            *int64      `json:"trigger_price,omitempty"`     
	Validity                *string     `json:"validity,omitempty"`          
	ValidityTTL             *int64      `json:"validity_ttl,omitempty"`      
	Variety                 *string     `json:"variety,omitempty"`           
}

type Meta struct {
	Iceberg *Iceberg `json:"iceberg,omitempty"`
}

type Iceberg struct {
	Leg               *int64 `json:"leg,omitempty"`               
	LegQuantity       *int64 `json:"leg_quantity,omitempty"`      
	Legs              *int64 `json:"legs,omitempty"`              
	RemainingQuantity *int64 `json:"remaining_quantity,omitempty"`
	TotalQuantity     *int64 `json:"total_quantity,omitempty"`    
}
