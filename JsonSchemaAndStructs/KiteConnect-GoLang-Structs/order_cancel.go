// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderCancel, err := UnmarshalOrderCancel(bytes)
//    bytes, err = orderCancel.Marshal()

package OrderCancel

import "encoding/json"

func UnmarshalOrderCancel(data []byte) (OrderCancel, error) {
	var r OrderCancel
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderCancel) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderCancel struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	OrderID *string `json:"order_id,omitempty"`
}
