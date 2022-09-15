package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class TriggerRangeProperties {
    private NseInfy data;
    private InstrumentToken status;

    @JsonProperty("data")
    public NseInfy getData() { return data; }
    @JsonProperty("data")
    public void setData(NseInfy value) { this.data = value; }

    @JsonProperty("status")
    public InstrumentToken getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(InstrumentToken value) { this.status = value; }
}
