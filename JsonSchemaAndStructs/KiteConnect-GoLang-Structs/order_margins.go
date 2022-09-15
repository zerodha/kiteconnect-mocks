// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderMargins, err := UnmarshalOrderMargins(bytes)
//    bytes, err = orderMargins.Marshal()

package main

import "encoding/json"

func UnmarshalOrderMargins(data []byte) (OrderMargins, error) {
	var r OrderMargins
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderMargins) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderMargins struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum        Datum             `json:"Datum"`       
	OrderMargins OrderMarginsClass `json:"OrderMargins"`
	Pnl          PnlClass          `json:"Pnl"`         
}

type Datum struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DatumProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DatumProperties struct {
	Additional    Additional `json:"additional"`    
	Bo            Additional `json:"bo"`            
	Cash          Additional `json:"cash"`          
	Exchange      Additional `json:"exchange"`      
	Exposure      Additional `json:"exposure"`      
	OptionPremium Additional `json:"option_premium"`
	Pnl           Pnl        `json:"pnl"`           
	Span          Additional `json:"span"`          
	Total         Additional `json:"total"`         
	Tradingsymbol Additional `json:"tradingsymbol"` 
	Type          Additional `json:"type"`          
	Var           Additional `json:"var"`           
}

type Additional struct {
	Type Type `json:"type"`
}

type Pnl struct {
	Ref string `json:"$ref"`
}

type OrderMarginsClass struct {
	AdditionalProperties bool                   `json:"additionalProperties"`
	Properties           OrderMarginsProperties `json:"properties"`          
	Required             []string               `json:"required"`            
	Title                string                 `json:"title"`               
	Type                 string                 `json:"type"`                
}

type OrderMarginsProperties struct {
	Data   Data       `json:"data"`  
	Status Additional `json:"status"`
}

type Data struct {
	Items Pnl    `json:"items"`
	Type  string `json:"type"` 
}

type PnlClass struct {
	AdditionalProperties bool          `json:"additionalProperties"`
	Properties           PnlProperties `json:"properties"`          
	Required             []string      `json:"required"`            
	Title                string        `json:"title"`               
	Type                 string        `json:"type"`                
}

type PnlProperties struct {
	Realised   Additional `json:"realised"`  
	Unrealised Additional `json:"unrealised"`
}

type Type string
const (
	Integer Type = "integer"
	Number Type = "number"
	String Type = "string"
)
