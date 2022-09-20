// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFOrders, err := UnmarshalMFOrders(bytes)
//    bytes, err = mFOrders.Marshal()

package MfOrders

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
	Data   []Datum `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Datum struct {
	Amount            *int64      `json:"amount,omitempty"`          
	AveragePrice      *int64      `json:"average_price,omitempty"`   
	ExchangeOrderID   *string     `json:"exchange_order_id"`         
	ExchangeTimestamp *string     `json:"exchange_timestamp"`        
	Folio             interface{} `json:"folio"`                     
	Fund              *string     `json:"fund,omitempty"`            
	LastPrice         *float64    `json:"last_price,omitempty"`      
	LastPriceDate     *string     `json:"last_price_date,omitempty"` 
	OrderID           *string     `json:"order_id,omitempty"`        
	OrderTimestamp    *string     `json:"order_timestamp,omitempty"` 
	PlacedBy          *string     `json:"placed_by,omitempty"`       
	PurchaseType      *string     `json:"purchase_type,omitempty"`   
	Quantity          *int64      `json:"quantity,omitempty"`        
	SettlementID      *string     `json:"settlement_id"`             
	Status            *string     `json:"status,omitempty"`          
	StatusMessage     *string     `json:"status_message,omitempty"`  
	Tag               *string     `json:"tag"`                       
	Tradingsymbol     *string     `json:"tradingsymbol,omitempty"`   
	TransactionType   *string     `json:"transaction_type,omitempty"`
	Variety           *string     `json:"variety,omitempty"`         
}
