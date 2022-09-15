// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    profile, err := UnmarshalProfile(bytes)
//    bytes, err = profile.Marshal()

package main

import "encoding/json"

func UnmarshalProfile(data []byte) (Profile, error) {
	var r Profile
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Profile) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Profile struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data    Data         `json:"Data"`   
	Meta    MetaClass    `json:"Meta"`   
	Profile ProfileClass `json:"Profile"`
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	AvatarURL     AvatarURL `json:"avatar_url"`    
	Broker        AvatarURL `json:"broker"`        
	Email         AvatarURL `json:"email"`         
	Exchanges     Exchanges `json:"exchanges"`     
	Meta          Meta      `json:"meta"`          
	OrderTypes    Exchanges `json:"order_types"`   
	Products      Exchanges `json:"products"`      
	UserID        AvatarURL `json:"user_id"`       
	UserName      AvatarURL `json:"user_name"`     
	UserShortname AvatarURL `json:"user_shortname"`
	UserType      AvatarURL `json:"user_type"`     
}

type AvatarURL struct {
	Type Type `json:"type"`
}

type Exchanges struct {
	Items AvatarURL `json:"items"`
	Type  string    `json:"type"` 
}

type Meta struct {
	Ref string `json:"$ref"`
}

type MetaClass struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           MetaProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type MetaProperties struct {
	DematConsent AvatarURL `json:"demat_consent"`
}

type ProfileClass struct {
	AdditionalProperties bool              `json:"additionalProperties"`
	Properties           ProfileProperties `json:"properties"`          
	Required             []string          `json:"required"`            
	Title                string            `json:"title"`               
	Type                 string            `json:"type"`                
}

type ProfileProperties struct {
	Data   Meta      `json:"data"`  
	Status AvatarURL `json:"status"`
}

type Type string
const (
	Null Type = "null"
	String Type = "string"
)
