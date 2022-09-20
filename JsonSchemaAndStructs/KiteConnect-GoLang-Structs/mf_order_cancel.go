// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFOrderCancel, err := UnmarshalMFOrderCancel(bytes)
//    bytes, err = mFOrderCancel.Marshal()

package MfOrderCancel

import "encoding/json"

func UnmarshalMFOrderCancel(data []byte) (MFOrderCancel, error) {
	var r MFOrderCancel
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFOrderCancel) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFOrderCancel struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	OrderID *string `json:"order_id,omitempty"`
}
