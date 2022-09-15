package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class BasketMarginsProperties {
    private Data data;
    private Status status;

    @JsonProperty("data")
    public Data getData() { return data; }
    @JsonProperty("data")
    public void setData(Data value) { this.data = value; }

    @JsonProperty("status")
    public Status getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Status value) { this.status = value; }
}
