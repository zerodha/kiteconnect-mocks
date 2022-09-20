// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    instrumentsAll, err := UnmarshalInstrumentsAll(bytes)
//    bytes, err = instrumentsAll.Marshal()

package InstrumentsAll

import "encoding/json"

type InstrumentsAll []InstrumentsAllElement

func UnmarshalInstrumentsAll(data []byte) (InstrumentsAll, error) {
	var r InstrumentsAll
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *InstrumentsAll) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type InstrumentsAllElement struct {
	Exchange        *Exchange       `json:"exchange,omitempty"`        
	ExchangeToken   *int64          `json:"exchange_token,omitempty"`  
	Expiry          *string         `json:"expiry,omitempty"`          
	InstrumentToken *int64          `json:"instrument_token,omitempty"`
	InstrumentType  *InstrumentType `json:"instrument_type,omitempty"` 
	LastPrice       *float64        `json:"last_price,omitempty"`      
	LotSize         *int64          `json:"lot_size,omitempty"`        
	Name            *string         `json:"name,omitempty"`            
	Segment         *Segment        `json:"segment,omitempty"`         
	Strike          *int64          `json:"strike,omitempty"`          
	TickSize        *float64        `json:"tick_size,omitempty"`       
	Tradingsymbol   *string         `json:"tradingsymbol,omitempty"`   
}

type Exchange string
const (
	ExchangeBSE Exchange = "BSE"
	ExchangeNSE Exchange = "NSE"
	Nfo Exchange = "NFO"
)

type InstrumentType string
const (
	Ce InstrumentType = "CE"
	Eq InstrumentType = "EQ"
	PE InstrumentType = "PE"
)

type Segment string
const (
	NfoOpt Segment = "NFO-OPT"
	SegmentBSE Segment = "BSE"
	SegmentNSE Segment = "NSE"
)
