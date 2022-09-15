// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    gttDeleteOrder, err := UnmarshalGttDeleteOrder(bytes)
//    bytes, err = gttDeleteOrder.Marshal()

package main

import "encoding/json"

func UnmarshalGttDeleteOrder(data []byte) (GttDeleteOrder, error) {
	var r GttDeleteOrder
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GttDeleteOrder) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GttDeleteOrder struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data           Data                `json:"Data"`          
	GttDeleteOrder GttDeleteOrderClass `json:"GttDeleteOrder"`
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

type GttDeleteOrderClass struct {
	AdditionalProperties bool                     `json:"additionalProperties"`
	Properties           GttDeleteOrderProperties `json:"properties"`          
	Required             []string                 `json:"required"`            
	Title                string                   `json:"title"`               
	Type                 string                   `json:"type"`                
}

type GttDeleteOrderProperties struct {
	Data   DataClass `json:"data"`  
	Status TriggerID `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
