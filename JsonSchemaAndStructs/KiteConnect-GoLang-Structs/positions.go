// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    positions, err := UnmarshalPositions(bytes)
//    bytes, err = positions.Marshal()

package main

import "encoding/json"

func UnmarshalPositions(data []byte) (Positions, error) {
	var r Positions
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Positions) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Positions struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data      Data           `json:"Data"`     
	Day       DayClass       `json:"Day"`      
	Positions PositionsClass `json:"Positions"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	Day Day `json:"day"`
	Net Day `json:"net"`
}

type Day struct {
	Items DataClass `json:"items"`
	Type  string    `json:"type"` 
}

type DataClass struct {
	Ref string `json:"$ref"`
}

type DayClass struct {
	AdditionalProperties bool                `json:"additionalProperties"`
	Properties           map[string]Property `json:"properties"`          
	Required             []string            `json:"required"`            
	Title                string              `json:"title"`               
	Type                 string              `json:"type"`                
}

type Property struct {
	Type Type `json:"type"`
}

type PositionsClass struct {
	AdditionalProperties bool                `json:"additionalProperties"`
	Properties           PositionsProperties `json:"properties"`          
	Required             []string            `json:"required"`            
	Title                string              `json:"title"`               
	Type                 string              `json:"type"`                
}

type PositionsProperties struct {
	Data   DataClass `json:"data"`  
	Status Property  `json:"status"`
}

type Type string
const (
	Integer Type = "integer"
	Number Type = "number"
	String Type = "string"
)
