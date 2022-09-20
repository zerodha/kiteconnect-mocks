// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSips, err := UnmarshalMFSips(bytes)
//    bytes, err = mFSips.Marshal()

package MfSips

import "encoding/json"

func UnmarshalMFSips(data []byte) (MFSips, error) {
	var r MFSips
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFSips) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFSips struct {
	Data []Datum `json:"data,omitempty"`
}

type Datum struct {
	CompletedInstalments *int64           `json:"completed_instalments,omitempty"`
	Created              *string          `json:"created,omitempty"`              
	DividendType         *string          `json:"dividend_type,omitempty"`        
	Frequency            *string          `json:"frequency,omitempty"`            
	Fund                 *string          `json:"fund,omitempty"`                 
	InstalmentAmount     *int64           `json:"instalment_amount,omitempty"`    
	InstalmentDay        *int64           `json:"instalment_day,omitempty"`       
	Instalments          *int64           `json:"instalments,omitempty"`          
	LastInstalment       *string          `json:"last_instalment,omitempty"`      
	NextInstalment       *string          `json:"next_instalment,omitempty"`      
	PendingInstalments   *int64           `json:"pending_instalments,omitempty"`  
	SIPID                *string          `json:"sip_id,omitempty"`               
	SIPRegNum            *string          `json:"sip_reg_num"`                    
	SIPType              *string          `json:"sip_type,omitempty"`             
	Status               *string          `json:"status,omitempty"`               
	StepUp               map[string]int64 `json:"step_up,omitempty"`              
	Tag                  *string          `json:"tag,omitempty"`                  
	Tradingsymbol        *string          `json:"tradingsymbol,omitempty"`        
	TransactionType      *string          `json:"transaction_type,omitempty"`     
	TriggerPrice         *int64           `json:"trigger_price,omitempty"`        
}
