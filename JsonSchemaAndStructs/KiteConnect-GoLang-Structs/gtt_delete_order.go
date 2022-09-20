// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    gttDeleteOrder, err := UnmarshalGttDeleteOrder(bytes)
//    bytes, err = gttDeleteOrder.Marshal()

package GttDeleteOrder

import "encoding/json"

func UnmarshalGttDeleteOrder(data []byte) (GttDeleteOrder, error) {
	var r GttDeleteOrder
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GttDeleteOrder) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GttDeleteOrder struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	TriggerID *int64 `json:"trigger_id,omitempty"`
}
