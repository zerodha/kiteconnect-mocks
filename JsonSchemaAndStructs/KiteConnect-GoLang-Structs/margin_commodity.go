// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    marginCommodity, err := UnmarshalMarginCommodity(bytes)
//    bytes, err = marginCommodity.Marshal()

package main

import "encoding/json"

func UnmarshalMarginCommodity(data []byte) (MarginCommodity, error) {
	var r MarginCommodity
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MarginCommodity) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MarginCommodity struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Available       Available            `json:"Available"`      
	Data            Data                 `json:"Data"`           
	MarginCommodity MarginCommodityClass `json:"MarginCommodity"`
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
	Available AvailableClass `json:"available"`
	Enabled   AdhocMargin    `json:"enabled"`  
	Net       AdhocMargin    `json:"net"`      
	Utilised  Utilised       `json:"utilised"` 
}

type AvailableClass struct {
	Ref string `json:"$ref"`
}

type Utilised struct {
	AdditionalProperties AdhocMargin `json:"additionalProperties"`
	Type                 string      `json:"type"`                
}

type MarginCommodityClass struct {
	AdditionalProperties bool                      `json:"additionalProperties"`
	Properties           MarginCommodityProperties `json:"properties"`          
	Required             []string                  `json:"required"`            
	Title                string                    `json:"title"`               
	Type                 string                    `json:"type"`                
}

type MarginCommodityProperties struct {
	Data   AvailableClass `json:"data"`  
	Status AdhocMargin    `json:"status"`
}
