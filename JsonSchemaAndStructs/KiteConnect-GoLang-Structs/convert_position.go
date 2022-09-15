// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    convertPosition, err := UnmarshalConvertPosition(bytes)
//    bytes, err = convertPosition.Marshal()

package main

import "encoding/json"

func UnmarshalConvertPosition(data []byte) (ConvertPosition, error) {
	var r ConvertPosition
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ConvertPosition) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type ConvertPosition struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	ConvertPosition ConvertPositionClass `json:"ConvertPosition"`
}

type ConvertPositionClass struct {
	AdditionalProperties bool       `json:"additionalProperties"`
	Properties           Properties `json:"properties"`          
	Required             []string   `json:"required"`            
	Title                string     `json:"title"`               
	Type                 string     `json:"type"`                
}

type Properties struct {
	Data   Data `json:"data"`  
	Status Data `json:"status"`
}

type Data struct {
	Type string `json:"type"`
}
