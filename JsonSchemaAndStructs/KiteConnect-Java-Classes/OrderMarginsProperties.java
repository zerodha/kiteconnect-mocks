package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class OrderMarginsProperties {
    private Data data;
    private Additional status;

    @JsonProperty("data")
    public Data getData() { return data; }
    @JsonProperty("data")
    public void setData(Data value) { this.data = value; }

    @JsonProperty("status")
    public Additional getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Additional value) { this.status = value; }
}
