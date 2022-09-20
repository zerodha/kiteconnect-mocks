// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSIPPlace, err := UnmarshalMFSIPPlace(bytes)
//    bytes, err = mFSIPPlace.Marshal()

package MfSipPlace

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
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	SIPID *string `json:"sip_id,omitempty"`
}
