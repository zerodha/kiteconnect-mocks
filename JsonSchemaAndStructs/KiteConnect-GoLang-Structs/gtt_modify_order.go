// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    gttModifyOrder, err := UnmarshalGttModifyOrder(bytes)
//    bytes, err = gttModifyOrder.Marshal()

package main

import "encoding/json"

func UnmarshalGttModifyOrder(data []byte) (GttModifyOrder, error) {
	var r GttModifyOrder
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GttModifyOrder) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GttModifyOrder struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data           Data                `json:"Data"`          
	GttModifyOrder GttModifyOrderClass `json:"GttModifyOrder"`
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

type GttModifyOrderClass struct {
	AdditionalProperties bool                     `json:"additionalProperties"`
	Properties           GttModifyOrderProperties `json:"properties"`          
	Required             []string                 `json:"required"`            
	Title                string                   `json:"title"`               
	Type                 string                   `json:"type"`                
}

type GttModifyOrderProperties struct {
	Data   DataClass `json:"data"`  
	Status TriggerID `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
