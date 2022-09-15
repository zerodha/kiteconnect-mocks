// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    mFSIPInfo, err := UnmarshalMFSIPInfo(bytes)
//    bytes, err = mFSIPInfo.Marshal()

package main

import "encoding/json"

func UnmarshalMFSIPInfo(data []byte) (MFSIPInfo, error) {
	var r MFSIPInfo
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *MFSIPInfo) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type MFSIPInfo struct {
	Ref         string      `json:"$ref"`       
	Schema      string      `json:"$schema"`    
	Definitions Definitions `json:"definitions"`
}

type Definitions struct {
	Data      Data           `json:"Data"`     
	MFSIPInfo MFSIPInfoClass `json:"MFSIPInfo"`
	StepUp    StepUpClass    `json:"StepUp"`   
}

type Data struct {
	AdditionalProperties bool           `json:"additionalProperties"`
	Properties           DataProperties `json:"properties"`          
	Required             []string       `json:"required"`            
	Title                string         `json:"title"`               
	Type                 string         `json:"type"`                
}

type DataProperties struct {
	CompletedInstalments CompletedInstalments `json:"completed_instalments"`
	Created              Created              `json:"created"`              
	DividendType         CompletedInstalments `json:"dividend_type"`        
	Frequency            CompletedInstalments `json:"frequency"`            
	Fund                 CompletedInstalments `json:"fund"`                 
	FundSource           CompletedInstalments `json:"fund_source"`          
	InstalmentAmount     CompletedInstalments `json:"instalment_amount"`    
	InstalmentDay        CompletedInstalments `json:"instalment_day"`       
	Instalments          CompletedInstalments `json:"instalments"`          
	LastInstalment       Created              `json:"last_instalment"`      
	NextInstalment       Created              `json:"next_instalment"`      
	PendingInstalments   CompletedInstalments `json:"pending_instalments"`  
	SIPID                CompletedInstalments `json:"sip_id"`               
	SIPRegNum            CompletedInstalments `json:"sip_reg_num"`          
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
	Format string `json:"format"`
	Type   Type   `json:"type"`  
}

type StepUp struct {
	Ref string `json:"$ref"`
}

type MFSIPInfoClass struct {
	AdditionalProperties bool                `json:"additionalProperties"`
	Properties           MFSIPInfoProperties `json:"properties"`          
	Required             []string            `json:"required"`            
	Title                string              `json:"title"`               
	Type                 string              `json:"type"`                
}

type MFSIPInfoProperties struct {
	Data   StepUp               `json:"data"`  
	Status CompletedInstalments `json:"status"`
}

type StepUpClass struct {
	AdditionalProperties bool             `json:"additionalProperties"`
	Properties           StepUpProperties `json:"properties"`          
	Required             []string         `json:"required"`            
	Title                string           `json:"title"`               
	Type                 string           `json:"type"`                
}

type StepUpProperties struct {
	The1502 CompletedInstalments `json:"15-02"`
}

type Type string
const (
	Integer Type = "integer"
	Null Type = "null"
	Number Type = "number"
	String Type = "string"
)
