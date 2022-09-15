// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFHoldings, err := UnmarshalMFHoldings(bytes)
//    bytes, err = mFHoldings.Marshal()

package main

import "encoding/json"

func UnmarshalMFHoldings(data []byte) (MFHoldings, error) {
	var r MFHoldings
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFHoldings) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFHoldings struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum      Datum           `json:"Datum"`     
	MFHoldings MFHoldingsClass `json:"MFHoldings"`
}

type Datum struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DatumProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DatumProperties struct {
	AveragePrice    AveragePrice `json:"average_price"`   
	Folio           AveragePrice `json:"folio"`           
	Fund            AveragePrice `json:"fund"`            
	LastPrice       AveragePrice `json:"last_price"`      
	LastPriceDate   AveragePrice `json:"last_price_date"` 
	PledgedQuantity AveragePrice `json:"pledged_quantity"`
	Pnl             AveragePrice `json:"pnl"`             
	Quantity        AveragePrice `json:"quantity"`        
	Tradingsymbol   AveragePrice `json:"tradingsymbol"`   
}

type AveragePrice struct {
	Type Type `json:"type"`
}

type MFHoldingsClass struct {
	AdditionalProperties bool                 `json:"additionalProperties"`
	Properties           MFHoldingsProperties `json:"properties"`          
	Required             []string             `json:"required"`            
	Title                string               `json:"title"`               
	Type                 string               `json:"type"`                
}

type MFHoldingsProperties struct {
	Data   Data         `json:"data"`  
	Status AveragePrice `json:"status"`
}

type Data struct {
	Items Items  `json:"items"`
	Type  string `json:"type"` 
}

type Items struct {
	Ref string `json:"$ref"`
}

type Type string
const (
	Integer Type = "integer"
	Number Type = "number"
	String Type = "string"
)
