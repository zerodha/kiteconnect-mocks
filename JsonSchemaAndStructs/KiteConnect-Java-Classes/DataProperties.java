package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class DataProperties {
    private NseInfy nseInfy;
    private NseInfy nseReliance;

    @JsonProperty("NSE:INFY")
    public NseInfy getNseInfy() { return nseInfy; }
    @JsonProperty("NSE:INFY")
    public void setNseInfy(NseInfy value) { this.nseInfy = value; }

    @JsonProperty("NSE:RELIANCE")
    public NseInfy getNseReliance() { return nseReliance; }
    @JsonProperty("NSE:RELIANCE")
    public void setNseReliance(NseInfy value) { this.nseReliance = value; }
}
