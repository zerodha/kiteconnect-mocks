// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    orderResponse, err := UnmarshalOrderResponse(bytes)
//    bytes, err = orderResponse.Marshal()

package OrderResponse

import "encoding/json"

func UnmarshalOrderResponse(data []byte) (OrderResponse, error) {
	var r OrderResponse
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OrderResponse) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type OrderResponse struct {
	Data   *Data   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}

type Data struct {
	OrderID *string `json:"order_id,omitempty"`
}
