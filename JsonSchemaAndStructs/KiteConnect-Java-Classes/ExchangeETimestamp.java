package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class ExchangeETimestamp {
    private OrderTimestamp[] anyOf;

    @JsonProperty("anyOf")
    public OrderTimestamp[] getAnyOf() { return anyOf; }
    @JsonProperty("anyOf")
    public void setAnyOf(OrderTimestamp[] value) { this.anyOf = value; }
}
