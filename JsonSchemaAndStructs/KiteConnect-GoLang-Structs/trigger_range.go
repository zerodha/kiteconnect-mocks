// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    triggerRange, err := UnmarshalTriggerRange(bytes)
//    bytes, err = triggerRange.Marshal()

package main

import "encoding/json"

func UnmarshalTriggerRange(data []byte) (TriggerRange, error) {
	var r TriggerRange
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TriggerRange) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type TriggerRange struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data         Data              `json:"Data"`        
	Nse          Nse               `json:"Nse"`         
	TriggerRange TriggerRangeClass `json:"TriggerRange"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	NseInfy     NseInfy `json:"NSE:INFY"`    
	NseReliance NseInfy `json:"NSE:RELIANCE"`
}

type NseInfy struct {
	Ref string `json:"$ref"`
}

type Nse struct {
	AdditionalProperties bool          `json:"additionalProperties"`
	Properties           NseProperties `json:"properties"`          
	Required             []string      `json:"required"`            
	Title                string        `json:"title"`               
	Type                 string        `json:"type"`                
}

type NseProperties struct {
	InstrumentToken InstrumentToken `json:"instrument_token"`
	Lower           InstrumentToken `json:"lower"`           
	Upper           InstrumentToken `json:"upper"`           
}

type InstrumentToken struct {
	Type string `json:"type"`
}

type TriggerRangeClass struct {
	AdditionalProperties bool                   `json:"additionalProperties"`
	Properties           TriggerRangeProperties `json:"properties"`          
	Required             []string               `json:"required"`            
	Title                string                 `json:"title"`               
	Type                 string                 `json:"type"`                
}

type TriggerRangeProperties struct {
	Data   NseInfy         `json:"data"`  
	Status InstrumentToken `json:"status"`
}
