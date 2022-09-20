// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderModify, err := UnmarshalOrderModify(bytes)
//    bytes, err = orderModify.Marshal()

package OrderModify

import "encoding/json"

func UnmarshalOrderModify(data []byte) (OrderModify, error) {
	var r OrderModify
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderModify) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderModify struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	OrderID *string `json:"order_id,omitempty"`
}
