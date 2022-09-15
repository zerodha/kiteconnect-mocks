package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class QuoteProperties {
    private NseInfy data;
    private Orders status;

    @JsonProperty("data")
    public NseInfy getData() { return data; }
    @JsonProperty("data")
    public void setData(NseInfy value) { this.data = value; }

    @JsonProperty("status")
    public Orders getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Orders value) { this.status = value; }
}
