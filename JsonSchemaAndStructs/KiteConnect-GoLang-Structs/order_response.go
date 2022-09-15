// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderResponse, err := UnmarshalOrderResponse(bytes)
//    bytes, err = orderResponse.Marshal()

package main

import "encoding/json"

func UnmarshalOrderResponse(data []byte) (OrderResponse, error) {
	var r OrderResponse
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderResponse) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderResponse struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data          Data               `json:"Data"`         
	OrderResponse OrderResponseClass `json:"OrderResponse"`
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

type OrderResponseClass struct {
	AdditionalProperties bool                    `json:"additionalProperties"`
	Properties           OrderResponseProperties `json:"properties"`          
	Required             []string                `json:"required"`            
	Title                string                  `json:"title"`               
	Type                 string                  `json:"type"`                
}

type OrderResponseProperties struct {
	Data   DataClass `json:"data"`  
	Status OrderID   `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
