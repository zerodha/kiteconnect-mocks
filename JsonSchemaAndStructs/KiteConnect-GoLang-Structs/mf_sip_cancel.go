// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSIPCancel, err := UnmarshalMFSIPCancel(bytes)
//    bytes, err = mFSIPCancel.Marshal()

package MfSipCancel

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
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	SIPID *string `json:"sip_id,omitempty"`
}
