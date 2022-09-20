// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    holdingsAuth, err := UnmarshalHoldingsAuth(bytes)
//    bytes, err = holdingsAuth.Marshal()

package HoldingsAuth

import "encoding/json"

func UnmarshalHoldingsAuth(data []byte) (HoldingsAuth, error) {
	var r HoldingsAuth
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *HoldingsAuth) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type HoldingsAuth struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	RequestID *string `json:"request_id,omitempty"`
}
