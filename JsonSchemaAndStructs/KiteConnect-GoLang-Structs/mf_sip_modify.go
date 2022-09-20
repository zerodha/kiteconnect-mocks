// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSIPModify, err := UnmarshalMFSIPModify(bytes)
//    bytes, err = mFSIPModify.Marshal()

package MfSipModify

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
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	SIPID *string `json:"sip_id,omitempty"`
}
