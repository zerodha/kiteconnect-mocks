// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    sessionLogout, err := UnmarshalSessionLogout(bytes)
//    bytes, err = sessionLogout.Marshal()

package SessionLogout

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
	Data   *bool   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}
