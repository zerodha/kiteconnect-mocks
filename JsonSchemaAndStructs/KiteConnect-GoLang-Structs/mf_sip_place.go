// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSIPPlace, err := UnmarshalMFSIPPlace(bytes)
//    bytes, err = mFSIPPlace.Marshal()

package main

import "encoding/json"

func UnmarshalMFSIPPlace(data []byte) (MFSIPPlace, error) {
	var r MFSIPPlace
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFSIPPlace) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFSIPPlace struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data       Data            `json:"Data"`      
	MFSIPPlace MFSIPPlaceClass `json:"MFSIPPlace"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	SIPID Sipid `json:"sip_id"`
}

type Sipid struct {
	Type string `json:"type"`
}

type MFSIPPlaceClass struct {
	AdditionalProperties bool                 `json:"additionalProperties"`
	Properties           MFSIPPlaceProperties `json:"properties"`          
	Required             []string             `json:"required"`            
	Title                string               `json:"title"`               
	Type                 string               `json:"type"`                
}

type MFSIPPlaceProperties struct {
	Data   DataClass `json:"data"`  
	Status Sipid     `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
