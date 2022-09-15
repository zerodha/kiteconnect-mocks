// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    ohlc, err := UnmarshalOhlc(bytes)
//    bytes, err = ohlc.Marshal()

package main

import "encoding/json"

func UnmarshalOhlc(data []byte) (Ohlc, error) {
	var r Ohlc
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Ohlc) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Ohlc struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data      Data           `json:"Data"`     
	NseInfy   NseInfyClass   `json:"NseInfy"`  
	Ohlc      OhlcClass      `json:"Ohlc"`     
	OhlcClass OhlcClassClass `json:"OhlcClass"`
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

type NseInfyClass struct {
	AdditionalProperties bool              `json:"additionalProperties"`
	Properties           NseInfyProperties `json:"properties"`          
	Required             []string          `json:"required"`            
	Title                string            `json:"title"`               
	Type                 string            `json:"type"`                
}

type NseInfyProperties struct {
	InstrumentToken InstrumentToken `json:"instrument_token"`
	LastPrice       InstrumentToken `json:"last_price"`      
	Ohlc            NseInfy         `json:"ohlc"`            
}

type InstrumentToken struct {
	Type string `json:"type"`
}

type OhlcClass struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           OhlcProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type OhlcProperties struct {
	Data   NseInfy         `json:"data"`  
	Status InstrumentToken `json:"status"`
}

type OhlcClassClass struct {
	AdditionalProperties bool                `json:"additionalProperties"`
	Properties           OhlcClassProperties `json:"properties"`          
	Required             []string            `json:"required"`            
	Title                string              `json:"title"`               
	Type                 string              `json:"type"`                
}

type OhlcClassProperties struct {
	Close InstrumentToken `json:"close"`
	High  InstrumentToken `json:"high"` 
	Low   InstrumentToken `json:"low"`  
	Open  InstrumentToken `json:"open"` 
}
