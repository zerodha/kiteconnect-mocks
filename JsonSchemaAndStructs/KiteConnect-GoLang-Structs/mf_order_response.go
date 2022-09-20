// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFOrderResponse, err := UnmarshalMFOrderResponse(bytes)
//    bytes, err = mFOrderResponse.Marshal()

package MfOrderResponse

import "encoding/json"

func UnmarshalMFOrderResponse(data []byte) (MFOrderResponse, error) {
	var r MFOrderResponse
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFOrderResponse) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFOrderResponse struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	OrderID *string `json:"order_id,omitempty"`
}
