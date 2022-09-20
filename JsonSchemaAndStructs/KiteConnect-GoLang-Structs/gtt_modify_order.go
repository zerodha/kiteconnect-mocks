// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    gttModifyOrder, err := UnmarshalGttModifyOrder(bytes)
//    bytes, err = gttModifyOrder.Marshal()

package GttModifyOrder

import "encoding/json"

func UnmarshalGttModifyOrder(data []byte) (GttModifyOrder, error) {
	var r GttModifyOrder
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GttModifyOrder) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GttModifyOrder struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	TriggerID *int64 `json:"trigger_id,omitempty"`
}
