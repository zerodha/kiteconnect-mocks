// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFOrderCancel, err := UnmarshalMFOrderCancel(bytes)
//    bytes, err = mFOrderCancel.Marshal()

package main

import "encoding/json"

func UnmarshalMFOrderCancel(data []byte) (MFOrderCancel, error) {
	var r MFOrderCancel
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFOrderCancel) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFOrderCancel struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data          Data               `json:"Data"`         
	MFOrderCancel MFOrderCancelClass `json:"MFOrderCancel"`
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

type MFOrderCancelClass struct {
	AdditionalProperties bool                    `json:"additionalProperties"`
	Properties           MFOrderCancelProperties `json:"properties"`          
	Required             []string                `json:"required"`            
	Title                string                  `json:"title"`               
	Type                 string                  `json:"type"`                
}

type MFOrderCancelProperties struct {
	Data   DataClass `json:"data"`  
	Status Status    `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}

type Status struct {
	Type string `json:"type"`
}
