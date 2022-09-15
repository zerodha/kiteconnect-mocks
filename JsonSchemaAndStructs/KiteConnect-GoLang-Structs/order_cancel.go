// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderCancel, err := UnmarshalOrderCancel(bytes)
//    bytes, err = orderCancel.Marshal()

package main

import "encoding/json"

func UnmarshalOrderCancel(data []byte) (OrderCancel, error) {
	var r OrderCancel
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderCancel) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderCancel struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data        Data             `json:"Data"`       
	OrderCancel OrderCancelClass `json:"OrderCancel"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	OrderID OrderID `json:"order_id"`
}

type OrderID struct {
	Type string `json:"type"`
}

type OrderCancelClass struct {
	AdditionalProperties bool                  `json:"additionalProperties"`
	Properties           OrderCancelProperties `json:"properties"`          
	Required             []string              `json:"required"`            
	Title                string                `json:"title"`               
	Type                 string                `json:"type"`                
}

type OrderCancelProperties struct {
	Data   DataClass `json:"data"`  
	Status OrderID   `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
