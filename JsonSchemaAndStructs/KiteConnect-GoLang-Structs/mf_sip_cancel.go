// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSIPCancel, err := UnmarshalMFSIPCancel(bytes)
//    bytes, err = mFSIPCancel.Marshal()

package main

import "encoding/json"

func UnmarshalMFSIPCancel(data []byte) (MFSIPCancel, error) {
	var r MFSIPCancel
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFSIPCancel) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFSIPCancel struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data        Data             `json:"Data"`       
	MFSIPCancel MFSIPCancelClass `json:"MFSIPCancel"`
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

type MFSIPCancelClass struct {
	AdditionalProperties bool                  `json:"additionalProperties"`
	Properties           MFSIPCancelProperties `json:"properties"`          
	Required             []string              `json:"required"`            
	Title                string                `json:"title"`               
	Type                 string                `json:"type"`                
}

type MFSIPCancelProperties struct {
	Data   DataClass `json:"data"`  
	Status Sipid     `json:"status"`
}

type DataClass struct {
	Ref string `json:"$ref"`
}
