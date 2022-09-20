// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFOrdersInfo, err := UnmarshalMFOrdersInfo(bytes)
//    bytes, err = mFOrdersInfo.Marshal()

package MfOrdersInfo

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
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	Amount            *int64      `json:"amount,omitempty"`          
	AveragePrice      *int64      `json:"average_price,omitempty"`   
	ExchangeOrderID   interface{} `json:"exchange_order_id"`         
	ExchangeTimestamp interface{} `json:"exchange_timestamp"`        
	Folio             interface{} `json:"folio"`                     
	Fund              *string     `json:"fund,omitempty"`            
	LastPrice         *float64    `json:"last_price,omitempty"`      
	LastPriceDate     *string     `json:"last_price_date,omitempty"` 
	OrderID           *string     `json:"order_id,omitempty"`        
	OrderTimestamp    *string     `json:"order_timestamp,omitempty"` 
	PlacedBy          *string     `json:"placed_by,omitempty"`       
	PurchaseType      *string     `json:"purchase_type,omitempty"`   
	Quantity          *int64      `json:"quantity,omitempty"`        
	SettlementID      interface{} `json:"settlement_id"`             
	Status            *string     `json:"status,omitempty"`          
	StatusMessage     *string     `json:"status_message,omitempty"`  
	Tag               interface{} `json:"tag"`                       
	Tradingsymbol     *string     `json:"tradingsymbol,omitempty"`   
	TransactionType   *string     `json:"transaction_type,omitempty"`
	Variety           *string     `json:"variety,omitempty"`         
}
