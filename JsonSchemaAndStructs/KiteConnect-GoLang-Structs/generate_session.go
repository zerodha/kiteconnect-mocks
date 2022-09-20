// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    generateSession, err := UnmarshalGenerateSession(bytes)
//    bytes, err = generateSession.Marshal()

package GenerateSession

import "encoding/json"

func UnmarshalGenerateSession(data []byte) (GenerateSession, error) {
	var r GenerateSession
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GenerateSession) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GenerateSession struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	AccessToken   *string  `json:"access_token,omitempty"`  
	APIKey        *string  `json:"api_key,omitempty"`       
	AvatarURL     *string  `json:"avatar_url,omitempty"`    
	Broker        *string  `json:"broker,omitempty"`        
	Email         *string  `json:"email,omitempty"`         
	Enctoken      *string  `json:"enctoken,omitempty"`      
	Exchanges     []string `json:"exchanges,omitempty"`     
	LoginTime     *string  `json:"login_time,omitempty"`    
	Meta          *Meta    `json:"meta,omitempty"`          
	OrderTypes    []string `json:"order_types,omitempty"`   
	Products      []string `json:"products,omitempty"`      
	PublicToken   *string  `json:"public_token,omitempty"`  
	RefreshToken  *string  `json:"refresh_token,omitempty"` 
	Silo          *string  `json:"silo,omitempty"`          
	UserID        *string  `json:"user_id,omitempty"`       
	UserName      *string  `json:"user_name,omitempty"`     
	UserShortname *string  `json:"user_shortname,omitempty"`
	UserType      *string  `json:"user_type,omitempty"`     
}

type Meta struct {
	DematConsent *string `json:"demat_consent,omitempty"`
}
