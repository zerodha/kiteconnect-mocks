// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    ltp, err := UnmarshalLtp(bytes)
//    bytes, err = ltp.Marshal()

package main

import "encoding/json"

func UnmarshalLtp(data []byte) (Ltp, error) {
	var r Ltp
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Ltp) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Ltp struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data    Data         `json:"Data"`   
	Ltp     LtpClass     `json:"Ltp"`    
	NseInfy NseInfyClass `json:"NseInfy"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	NseInfy NseInfy `json:"NSE:INFY"`
}

type NseInfy struct {
	Ref string `json:"$ref"`
}

type LtpClass struct {
	AdditionalProperties bool          `json:"additionalProperties"`
	Properties           LtpProperties `json:"properties"`          
	Required             []string      `json:"required"`            
	Title                string        `json:"title"`               
	Type                 string        `json:"type"`                
}

type LtpProperties struct {
	Data   NseInfy `json:"data"`  
	Status Status  `json:"status"`
}

type Status struct {
	Type string `json:"type"`
}

type NseInfyClass struct {
	AdditionalProperties bool              `json:"additionalProperties"`
	Properties           NseInfyProperties `json:"properties"`          
	Required             []string          `json:"required"`            
	Title                string            `json:"title"`               
	Type                 string            `json:"type"`                
}

type NseInfyProperties struct {
	InstrumentToken Status `json:"instrument_token"`
	LastPrice       Status `json:"last_price"`      
}
