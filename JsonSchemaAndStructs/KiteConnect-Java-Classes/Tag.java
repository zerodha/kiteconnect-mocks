package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Tag {
    private Amount[] anyOf;

    @JsonProperty("anyOf")
    public Amount[] getAnyOf() { return anyOf; }
    @JsonProperty("anyOf")
    public void setAnyOf(Amount[] value) { this.anyOf = value; }
}
