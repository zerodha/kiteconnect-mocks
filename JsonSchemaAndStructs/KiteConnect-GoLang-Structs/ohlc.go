// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    ohlc, err := UnmarshalOhlc(bytes)
//    bytes, err = ohlc.Marshal()

package Ohlc

import "encoding/json"

func UnmarshalOhlc(data []byte) (Ohlc, error) {
	var r Ohlc
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Ohlc) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type Ohlc struct {
	Data   map[string]Datum `json:"data,omitempty"`  
	Status *string          `json:"status,omitempty"`
}

type Datum struct {
	InstrumentToken *int64     `json:"instrument_token,omitempty"`
	LastPrice       *int64     `json:"last_price,omitempty"`      
	Ohlc            *OhlcClass `json:"ohlc,omitempty"`            
}

type OhlcClass struct {
	Close *float64 `json:"close,omitempty"`
	High  *float64 `json:"high,omitempty"` 
	Low   *float64 `json:"low,omitempty"`  
	Open  *float64 `json:"open,omitempty"` 
}
