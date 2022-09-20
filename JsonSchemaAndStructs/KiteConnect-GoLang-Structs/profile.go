// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    profile, err := UnmarshalProfile(bytes)
//    bytes, err = profile.Marshal()

package Profile

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
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	AvatarURL     interface{} `json:"avatar_url"`              
	Broker        *string     `json:"broker,omitempty"`        
	Email         *string     `json:"email,omitempty"`         
	Exchanges     []string    `json:"exchanges,omitempty"`     
	Meta          *Meta       `json:"meta,omitempty"`          
	OrderTypes    []string    `json:"order_types,omitempty"`   
	Products      []string    `json:"products,omitempty"`      
	UserID        *string     `json:"user_id,omitempty"`       
	UserName      *string     `json:"user_name,omitempty"`     
	UserShortname *string     `json:"user_shortname,omitempty"`
	UserType      *string     `json:"user_type,omitempty"`     
}

type Meta struct {
	DematConsent *string `json:"demat_consent,omitempty"`
}
