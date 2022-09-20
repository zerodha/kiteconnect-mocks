// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    convertPosition, err := UnmarshalConvertPosition(bytes)
//    bytes, err = convertPosition.Marshal()

package ConvertPosition

import "encoding/json"

func UnmarshalConvertPosition(data []byte) (ConvertPosition, error) {
	var r ConvertPosition
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ConvertPosition) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type ConvertPosition struct {
	Data   *bool   `json:"data,omitempty"`  
	Status *string `json:"status,omitempty"`
}
