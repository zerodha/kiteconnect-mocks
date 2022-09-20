// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderInfo, err := UnmarshalOrderInfo(bytes)
//    bytes, err = orderInfo.Marshal()

package OrderInfo

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
	Data   []Datum `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Datum struct {
	AveragePrice      *int64      `json:"average_price,omitempty"`     
	CancelledQuantity *int64      `json:"cancelled_quantity,omitempty"`
	DisclosedQuantity *int64      `json:"disclosed_quantity,omitempty"`
	Exchange          *string     `json:"exchange,omitempty"`          
	ExchangeOrderID   *string     `json:"exchange_order_id"`           
	ExchangeTimestamp *string     `json:"exchange_timestamp"`          
	FilledQuantity    *int64      `json:"filled_quantity,omitempty"`   
	InstrumentToken   *int64      `json:"instrument_token,omitempty"`  
	OrderID           *string     `json:"order_id,omitempty"`          
	OrderTimestamp    *string     `json:"order_timestamp,omitempty"`   
	OrderType         *string     `json:"order_type,omitempty"`        
	ParentOrderID     interface{} `json:"parent_order_id"`             
	PendingQuantity   *int64      `json:"pending_quantity,omitempty"`  
	PlacedBy          *string     `json:"placed_by,omitempty"`         
	Price             *float64    `json:"price,omitempty"`             
	Product           *string     `json:"product,omitempty"`           
	Quantity          *int64      `json:"quantity,omitempty"`          
	Status            *string     `json:"status,omitempty"`            
	StatusMessage     interface{} `json:"status_message"`              
	Tag               interface{} `json:"tag"`                         
	Tradingsymbol     *string     `json:"tradingsymbol,omitempty"`     
	TransactionType   *string     `json:"transaction_type,omitempty"`  
	TriggerPrice      *int64      `json:"trigger_price,omitempty"`     
	Validity          *string     `json:"validity,omitempty"`          
	Variety           *string     `json:"variety,omitempty"`           
}
