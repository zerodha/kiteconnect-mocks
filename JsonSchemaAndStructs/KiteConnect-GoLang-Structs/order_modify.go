// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderModify, err := UnmarshalOrderModify(bytes)
//    bytes, err = orderModify.Marshal()

package main

import "encoding/json"

func UnmarshalOrderModify(data []byte) (OrderModify, error) {
	var r OrderModify
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderModify) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderModify struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data        Data             `json:"Data"`       
	OrderModify OrderModifyClass `json:"OrderModify"`
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

type OrderModifyClass struct {
	AdditionalProperties bool                  `json:"additionalProperties"`
	Properties           OrderModifyProperties `json:"properties"`          
	Required             []string              `json:"required"`            
	Title                string                `json:"title"`               
	Type                 string                `json:"type"`                
}

type OrderModifyProperties struct {
	Data   DataClass `json:"data"`  
	Status OrderID   `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
