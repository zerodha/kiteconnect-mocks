package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class ExchangeOrderID {
    private AveragePrice[] anyOf;

    @JsonProperty("anyOf")
    public AveragePrice[] getAnyOf() { return anyOf; }
    @JsonProperty("anyOf")
    public void setAnyOf(AveragePrice[] value) { this.anyOf = value; }
}
