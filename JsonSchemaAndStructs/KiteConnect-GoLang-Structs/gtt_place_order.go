// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    gttPlaceOrder, err := UnmarshalGttPlaceOrder(bytes)
//    bytes, err = gttPlaceOrder.Marshal()

package GttPlaceOrder

import "encoding/json"

func UnmarshalGttPlaceOrder(data []byte) (GttPlaceOrder, error) {
	var r GttPlaceOrder
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GttPlaceOrder) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GttPlaceOrder struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	TriggerID *int64 `json:"trigger_id,omitempty"`
}
