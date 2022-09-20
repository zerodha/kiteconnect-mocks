// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    triggerRange, err := UnmarshalTriggerRange(bytes)
//    bytes, err = triggerRange.Marshal()

package TriggerRange

import "encoding/json"

func UnmarshalTriggerRange(data []byte) (TriggerRange, error) {
	var r TriggerRange
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TriggerRange) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type TriggerRange struct {
	Data   map[string]Datum `json:"data,omitempty"`  
	Status *string          `json:"status,omitempty"`
}

type Datum struct {
	InstrumentToken *int64   `json:"instrument_token,omitempty"`
	Lower           *float64 `json:"lower,omitempty"`           
	Upper           *float64 `json:"upper,omitempty"`           
}
