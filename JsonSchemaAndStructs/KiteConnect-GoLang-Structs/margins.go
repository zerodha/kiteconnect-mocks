// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    margins, err := UnmarshalMargins(bytes)
//    bytes, err = margins.Marshal()

package main

import "encoding/json"

func UnmarshalMargins(data []byte) (Margins, error) {
	var r Margins
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Margins) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Margins struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Available Available    `json:"Available"`
	Data      Data         `json:"Data"`     
	Ity       Ity          `json:"Ity"`      
	Margins   MarginsClass `json:"Margins"`  
}

type Available struct {
	AdditionalProperties bool                `json:"additionalProperties"`
	Properties           AvailableProperties `json:"properties"`          
	Required             []string            `json:"required"`            
	Title                string              `json:"title"`               
	Type                 string              `json:"type"`                
}

type AvailableProperties struct {
	AdhocMargin    AdhocMargin `json:"adhoc_margin"`   
	Cash           AdhocMargin `json:"cash"`           
	Collateral     AdhocMargin `json:"collateral"`     
	IntradayPayin  AdhocMargin `json:"intraday_payin"` 
	LiveBalance    AdhocMargin `json:"live_balance"`   
	OpeningBalance AdhocMargin `json:"opening_balance"`
}

type AdhocMargin struct {
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
	Commodity Commodity `json:"commodity"`
	Equity    Commodity `json:"equity"`   
}

type Commodity struct {
	Ref string `json:"$ref"`
}

type Ity struct {
	AdditionalProperties bool          `json:"additionalProperties"`
	Properties           ItyProperties `json:"properties"`          
	Required             []string      `json:"required"`            
	Title                string        `json:"title"`               
	Type                 string        `json:"type"`                
}

type ItyProperties struct {
	Available Commodity   `json:"available"`
	Enabled   AdhocMargin `json:"enabled"`  
	Net       AdhocMargin `json:"net"`      
	Utilised  Utilised    `json:"utilised"` 
}

type Utilised struct {
	AdditionalProperties AdhocMargin `json:"additionalProperties"`
	Type                 string      `json:"type"`                
}

type MarginsClass struct {
	AdditionalProperties bool              `json:"additionalProperties"`
	Properties           MarginsProperties `json:"properties"`          
	Required             []string          `json:"required"`            
	Title                string            `json:"title"`               
	Type                 string            `json:"type"`                
}

type MarginsProperties struct {
	Data   Commodity   `json:"data"`  
	Status AdhocMargin `json:"status"`
}
