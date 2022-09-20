// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    ltp, err := UnmarshalLtp(bytes)
//    bytes, err = ltp.Marshal()

package Ltp

import "encoding/json"

func UnmarshalLtp(data []byte) (Ltp, error) {
	var r Ltp
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Ltp) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Ltp struct {
	Data   map[string]Datum `json:"data,omitempty"`  
	Status *string          `json:"status,omitempty"`
}

type Datum struct {
	InstrumentToken *int64   `json:"instrument_token,omitempty"`
	LastPrice       *float64 `json:"last_price,omitempty"`      
}
