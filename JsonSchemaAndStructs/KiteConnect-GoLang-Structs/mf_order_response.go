// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFOrderResponse, err := UnmarshalMFOrderResponse(bytes)
//    bytes, err = mFOrderResponse.Marshal()

package main

import "encoding/json"

func UnmarshalMFOrderResponse(data []byte) (MFOrderResponse, error) {
	var r MFOrderResponse
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFOrderResponse) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFOrderResponse struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data            Data                 `json:"Data"`           
	MFOrderResponse MFOrderResponseClass `json:"MFOrderResponse"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	OrderID OrderID `json:"order_id"`
}

type OrderID struct {
	Format string `json:"format"`
	Type   string `json:"type"`  
}

type MFOrderResponseClass struct {
	AdditionalProperties bool                      `json:"additionalProperties"`
	Properties           MFOrderResponseProperties `json:"properties"`          
	Required             []string                  `json:"required"`            
	Title                string                    `json:"title"`               
	Type                 string                    `json:"type"`                
}

type MFOrderResponseProperties struct {
	Data   DataClass `json:"data"`  
	Status Status    `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}

type Status struct {
	Type string `json:"type"`
}
