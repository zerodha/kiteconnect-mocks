package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class SIPRegNum {
    private Created[] anyOf;

    @JsonProperty("anyOf")
    public Created[] getAnyOf() { return anyOf; }
    @JsonProperty("anyOf")
    public void setAnyOf(Created[] value) { this.anyOf = value; }
}
