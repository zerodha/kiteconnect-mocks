// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    historicalOi, err := UnmarshalHistoricalOi(bytes)
//    bytes, err = historicalOi.Marshal()

package main

import "encoding/json"

func UnmarshalHistoricalOi(data []byte) (HistoricalOi, error) {
	var r HistoricalOi
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *HistoricalOi) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type HistoricalOi struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Candle       Candle            `json:"Candle"`      
	Data         Data              `json:"Data"`        
	HistoricalOi HistoricalOiClass `json:"HistoricalOi"`
}

type Candle struct {
	AnyOf []AnyOf `json:"anyOf"`
	Title string  `json:"title"`
}

type AnyOf struct {
	Type string `json:"type"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	Candles Candles `json:"candles"`
}

type Candles struct {
	Items Items  `json:"items"`
	Type  string `json:"type"` 
}

type Items struct {
	Items DataClass `json:"items"`
	Type  string    `json:"type"` 
}

type DataClass struct {
	Ref string `json:"$ref"`
}

type HistoricalOiClass struct {
	AdditionalProperties bool                   `json:"additionalProperties"`
	Properties           HistoricalOiProperties `json:"properties"`          
	Required             []string               `json:"required"`            
	Title                string                 `json:"title"`               
	Type                 string                 `json:"type"`                
}

type HistoricalOiProperties struct {
	Data   DataClass `json:"data"`  
	Status AnyOf     `json:"status"`
}
