// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    holdingsAuth, err := UnmarshalHoldingsAuth(bytes)
//    bytes, err = holdingsAuth.Marshal()

package main

import "encoding/json"

func UnmarshalHoldingsAuth(data []byte) (HoldingsAuth, error) {
	var r HoldingsAuth
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *HoldingsAuth) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type HoldingsAuth struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data         Data              `json:"Data"`        
	HoldingsAuth HoldingsAuthClass `json:"HoldingsAuth"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	RequestID RequestID `json:"request_id"`
}

type RequestID struct {
	Type string `json:"type"`
}

type HoldingsAuthClass struct {
	AdditionalProperties bool                   `json:"additionalProperties"`
	Properties           HoldingsAuthProperties `json:"properties"`          
	Required             []string               `json:"required"`            
	Title                string                 `json:"title"`               
	Type                 string                 `json:"type"`                
}

type HoldingsAuthProperties struct {
	Data   DataClass `json:"data"`  
	Status RequestID `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
