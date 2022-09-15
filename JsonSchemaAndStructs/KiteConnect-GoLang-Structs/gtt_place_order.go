// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    gttPlaceOrder, err := UnmarshalGttPlaceOrder(bytes)
//    bytes, err = gttPlaceOrder.Marshal()

package main

import "encoding/json"

func UnmarshalGttPlaceOrder(data []byte) (GttPlaceOrder, error) {
	var r GttPlaceOrder
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GttPlaceOrder) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GttPlaceOrder struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data          Data               `json:"Data"`         
	GttPlaceOrder GttPlaceOrderClass `json:"GttPlaceOrder"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	TriggerID TriggerID `json:"trigger_id"`
}

type TriggerID struct {
	Type string `json:"type"`
}

type GttPlaceOrderClass struct {
	AdditionalProperties bool                    `json:"additionalProperties"`
	Properties           GttPlaceOrderProperties `json:"properties"`          
	Required             []string                `json:"required"`            
	Title                string                  `json:"title"`               
	Type                 string                  `json:"type"`                
}

type GttPlaceOrderProperties struct {
	Data   DataClass `json:"data"`  
	Status TriggerID `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
