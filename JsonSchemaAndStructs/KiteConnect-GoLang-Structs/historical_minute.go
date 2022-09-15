// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    historicalMinute, err := UnmarshalHistoricalMinute(bytes)
//    bytes, err = historicalMinute.Marshal()

package main

import "encoding/json"

func UnmarshalHistoricalMinute(data []byte) (HistoricalMinute, error) {
	var r HistoricalMinute
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *HistoricalMinute) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type HistoricalMinute struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Candle           Candle                `json:"Candle"`          
	Data             Data                  `json:"Data"`            
	HistoricalMinute HistoricalMinuteClass `json:"HistoricalMinute"`
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

type HistoricalMinuteClass struct {
	AdditionalProperties bool                       `json:"additionalProperties"`
	Properties           HistoricalMinuteProperties `json:"properties"`          
	Required             []string                   `json:"required"`            
	Title                string                     `json:"title"`               
	Type                 string                     `json:"type"`                
}

type HistoricalMinuteProperties struct {
	Data   DataClass `json:"data"`  
	Status AnyOf     `json:"status"`
}
