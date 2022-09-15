// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSips, err := UnmarshalMFSips(bytes)
//    bytes, err = mFSips.Marshal()

package main

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
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Datum  Datum       `json:"Datum"` 
	MFSips MFSipsClass `json:"MFSips"`
}

type Datum struct {
	AdditionalProperties bool            `json:"additionalProperties"`
	Properties           DatumProperties `json:"properties"`          
	Required             []string        `json:"required"`            
	Title                string          `json:"title"`               
	Type                 string          `json:"type"`                
}

type DatumProperties struct {
	CompletedInstalments CompletedInstalments `json:"completed_instalments"`
	Created              Created              `json:"created"`              
	DividendType         CompletedInstalments `json:"dividend_type"`        
	Frequency            CompletedInstalments `json:"frequency"`            
	Fund                 CompletedInstalments `json:"fund"`                 
	InstalmentAmount     CompletedInstalments `json:"instalment_amount"`    
	InstalmentDay        CompletedInstalments `json:"instalment_day"`       
	Instalments          CompletedInstalments `json:"instalments"`          
	LastInstalment       Created              `json:"last_instalment"`      
	NextInstalment       Created              `json:"next_instalment"`      
	PendingInstalments   CompletedInstalments `json:"pending_instalments"`  
	SIPID                CompletedInstalments `json:"sip_id"`               
	SIPRegNum            SIPRegNum            `json:"sip_reg_num"`          
	SIPType              CompletedInstalments `json:"sip_type"`             
	Status               CompletedInstalments `json:"status"`               
	StepUp               StepUp               `json:"step_up"`              
	Tag                  CompletedInstalments `json:"tag"`                  
	Tradingsymbol        CompletedInstalments `json:"tradingsymbol"`        
	TransactionType      CompletedInstalments `json:"transaction_type"`     
	TriggerPrice         CompletedInstalments `json:"trigger_price"`        
}

type CompletedInstalments struct {
	Type Type `json:"type"`
}

type Created struct {
	Format *string `json:"format,omitempty"`
	Type   string  `json:"type"`            
}

type SIPRegNum struct {
	AnyOf []Created `json:"anyOf"`
}

type StepUp struct {
	AdditionalProperties CompletedInstalments `json:"additionalProperties"`
	Type                 string               `json:"type"`                
}

type MFSipsClass struct {
	AdditionalProperties bool             `json:"additionalProperties"`
	Properties           MFSipsProperties `json:"properties"`          
	Required             []string         `json:"required"`            
	Title                string           `json:"title"`               
	Type                 string           `json:"type"`                
}

type MFSipsProperties struct {
	Data Data `json:"data"`
}

type Data struct {
	Items Items  `json:"items"`
	Type  string `json:"type"` 
}

type Items struct {
	Ref string `json:"$ref"`
}

type Type string
const (
	Integer Type = "integer"
	Number Type = "number"
	String Type = "string"
)
