// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    tickerLtp, err := UnmarshalTickerLtp(bytes)
//    bytes, err = tickerLtp.Marshal()

package TickerLtp

import "encoding/json"

type TickerLtp []TriggerRangeElement

func UnmarshalTickerLtp(data []byte) (TickerLtp, error) {
	var r TickerLtp
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TickerLtp) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type TriggerRangeElement struct {
	InstrumentToken *int64  `json:"instrument_token,omitempty"`
	LastPrice       *int64  `json:"last_price,omitempty"`      
	Mode            *string `json:"mode,omitempty"`            
	Tradable        *bool   `json:"tradable,omitempty"`        
}
