// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    basketMargins, err := UnmarshalBasketMargins(bytes)
//    bytes, err = basketMargins.Marshal()

package main

import "encoding/json"

func UnmarshalBasketMargins(data []byte) (BasketMargins, error) {
	var r BasketMargins
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *BasketMargins) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type BasketMargins struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	BasketMargins BasketMarginsClass `json:"BasketMargins"`
	Data          DataClass          `json:"Data"`         
	Final         Final              `json:"Final"`        
	Pnl           Pnl                `json:"Pnl"`          
}

type BasketMarginsClass struct {
	AdditionalProperties bool                    `json:"additionalProperties"`
	Properties           BasketMarginsProperties `json:"properties"`          
	Required             []string                `json:"required"`            
	Title                string                  `json:"title"`               
	Type                 string                  `json:"type"`                
}

type BasketMarginsProperties struct {
	Data   Data   `json:"data"`  
	Status Status `json:"status"`
}

type Data struct {
	Ref string `json:"$ref"`
}

type Status struct {
	Type Type `json:"type"`
}

type DataClass struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	Final   Data   `json:"final"`  
	Initial Data   `json:"initial"`
	Orders  Orders `json:"orders"` 
}

type Orders struct {
	Items Data   `json:"items"`
	Type  string `json:"type"` 
}

type Final struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           FinalProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type FinalProperties struct {
	Additional    Status `json:"additional"`    
	Bo            Status `json:"bo"`            
	Cash          Status `json:"cash"`          
	Exchange      Status `json:"exchange"`      
	Exposure      Status `json:"exposure"`      
	OptionPremium Status `json:"option_premium"`
	Pnl           Data   `json:"pnl"`           
	Span          Status `json:"span"`          
	Total         Status `json:"total"`         
	Tradingsymbol Status `json:"tradingsymbol"` 
	Type          Status `json:"type"`          
	Var           Status `json:"var"`           
}

type Pnl struct {
	AdditionalProperties bool          `json:"additionalProperties"`
	Properties           PnlProperties `json:"properties"`          
	Required             []string      `json:"required"`            
	Title                string        `json:"title"`               
	Type                 string        `json:"type"`                
}

type PnlProperties struct {
	Realised   Status `json:"realised"`  
	Unrealised Status `json:"unrealised"`
}

type Type string
const (
	Integer Type = "integer"
	Number Type = "number"
	String Type = "string"
)
