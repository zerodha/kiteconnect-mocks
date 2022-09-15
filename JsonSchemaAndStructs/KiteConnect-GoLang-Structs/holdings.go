// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    holdings, err := UnmarshalHoldings(bytes)
//    bytes, err = holdings.Marshal()

package main

import "encoding/json"

func UnmarshalHoldings(data []byte) (Holdings, error) {
	var r Holdings
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Holdings) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Holdings struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum    Datum         `json:"Datum"`   
	Holdings HoldingsClass `json:"Holdings"`
}

type Datum struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DatumProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DatumProperties struct {
	AuthorisedDate      AuthorisedDate     `json:"authorised_date"`      
	AuthorisedQuantity  AuthorisedQuantity `json:"authorised_quantity"`  
	AveragePrice        AuthorisedQuantity `json:"average_price"`        
	ClosePrice          AuthorisedQuantity `json:"close_price"`          
	CollateralQuantity  AuthorisedQuantity `json:"collateral_quantity"`  
	CollateralType      AuthorisedQuantity `json:"collateral_type"`      
	DayChange           AuthorisedQuantity `json:"day_change"`           
	DayChangePercentage AuthorisedQuantity `json:"day_change_percentage"`
	Discrepancy         AuthorisedQuantity `json:"discrepancy"`          
	Exchange            AuthorisedQuantity `json:"exchange"`             
	InstrumentToken     AuthorisedQuantity `json:"instrument_token"`     
	Isin                AuthorisedQuantity `json:"isin"`                 
	LastPrice           AuthorisedQuantity `json:"last_price"`           
	OpeningQuantity     AuthorisedQuantity `json:"opening_quantity"`     
	Pnl                 AuthorisedQuantity `json:"pnl"`                  
	Price               AuthorisedQuantity `json:"price"`                
	Product             AuthorisedQuantity `json:"product"`              
	Quantity            AuthorisedQuantity `json:"quantity"`             
	RealisedQuantity    AuthorisedQuantity `json:"realised_quantity"`    
	T1Quantity          AuthorisedQuantity `json:"t1_quantity"`          
	Tradingsymbol       AuthorisedQuantity `json:"tradingsymbol"`        
	UsedQuantity        AuthorisedQuantity `json:"used_quantity"`        
}

type AuthorisedDate struct {
	Format string `json:"format"`
	Type   Type   `json:"type"`  
}

type AuthorisedQuantity struct {
	Type Type `json:"type"`
}

type HoldingsClass struct {
	AdditionalProperties bool               `json:"additionalProperties"`
	Properties           HoldingsProperties `json:"properties"`          
	Required             []string           `json:"required"`            
	Title                string             `json:"title"`               
	Type                 string             `json:"type"`                
}

type HoldingsProperties struct {
	Data   Data               `json:"data"`  
	Status AuthorisedQuantity `json:"status"`
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
	Boolean Type = "boolean"
	Integer Type = "integer"
	Number Type = "number"
	String Type = "string"
)
