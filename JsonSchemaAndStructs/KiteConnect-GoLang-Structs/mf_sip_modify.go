// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSIPModify, err := UnmarshalMFSIPModify(bytes)
//    bytes, err = mFSIPModify.Marshal()

package main

import "encoding/json"

func UnmarshalMFSIPModify(data []byte) (MFSIPModify, error) {
	var r MFSIPModify
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFSIPModify) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFSIPModify struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data        Data             `json:"Data"`       
	MFSIPModify MFSIPModifyClass `json:"MFSIPModify"`
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

type MFSIPModifyClass struct {
	AdditionalProperties bool                  `json:"additionalProperties"`
	Properties           MFSIPModifyProperties `json:"properties"`          
	Required             []string              `json:"required"`            
	Title                string                `json:"title"`               
	Type                 string                `json:"type"`                
}

type MFSIPModifyProperties struct {
	Data   DataClass `json:"data"`  
	Status Sipid     `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
