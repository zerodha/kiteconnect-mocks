// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderTrades, err := UnmarshalOrderTrades(bytes)
//    bytes, err = orderTrades.Marshal()

package OrderTrades

import "encoding/json"

func UnmarshalOrderTrades(data []byte) (OrderTrades, error) {
	var r OrderTrades
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderTrades) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderTrades struct {
	Data   []Datum `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Datum struct {
	AveragePrice      *int64  `json:"average_price,omitempty"`     
	Exchange          *string `json:"exchange,omitempty"`          
	ExchangeOrderID   *string `json:"exchange_order_id,omitempty"` 
	ExchangeTimestamp *string `json:"exchange_timestamp,omitempty"`
	FillTimestamp     *string `json:"fill_timestamp,omitempty"`    
	InstrumentToken   *int64  `json:"instrument_token,omitempty"`  
	OrderID           *string `json:"order_id,omitempty"`          
	OrderTimestamp    *string `json:"order_timestamp,omitempty"`   
	Product           *string `json:"product,omitempty"`           
	Quantity          *int64  `json:"quantity,omitempty"`          
	TradeID           *string `json:"trade_id,omitempty"`          
	Tradingsymbol     *string `json:"tradingsymbol,omitempty"`     
	TransactionType   *string `json:"transaction_type,omitempty"`  
}
