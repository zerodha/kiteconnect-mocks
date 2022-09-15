package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class NseProperties {
    private InstrumentToken instrumentToken;
    private InstrumentToken lower;
    private InstrumentToken upper;

    @JsonProperty("instrument_token")
    public InstrumentToken getInstrumentToken() { return instrumentToken; }
    @JsonProperty("instrument_token")
    public void setInstrumentToken(InstrumentToken value) { this.instrumentToken = value; }

    @JsonProperty("lower")
    public InstrumentToken getLower() { return lower; }
    @JsonProperty("lower")
    public void setLower(InstrumentToken value) { this.lower = value; }

    @JsonProperty("upper")
    public InstrumentToken getUpper() { return upper; }
    @JsonProperty("upper")
    public void setUpper(InstrumentToken value) { this.upper = value; }
}
