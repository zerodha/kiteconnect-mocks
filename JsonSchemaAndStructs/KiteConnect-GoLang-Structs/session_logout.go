// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    sessionLogout, err := UnmarshalSessionLogout(bytes)
//    bytes, err = sessionLogout.Marshal()

package main

import "encoding/json"

func UnmarshalSessionLogout(data []byte) (SessionLogout, error) {
	var r SessionLogout
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *SessionLogout) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type SessionLogout struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	SessionLogout SessionLogoutClass `json:"SessionLogout"`
}

type SessionLogoutClass struct {
	AdditionalProperties bool       `json:"additionalProperties"`
	Properties           Properties `json:"properties"`          
	Required             []string   `json:"required"`            
	Title                string     `json:"title"`               
	Type                 string     `json:"type"`                
}

type Properties struct {
	Data   Data `json:"data"`  
	Status Data `json:"status"`
}

type Data struct {
	Type string `json:"type"`
}
