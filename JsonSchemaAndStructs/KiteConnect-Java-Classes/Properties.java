package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Properties {
    private Data data;
    private Data status;

    @JsonProperty("data")
    public Data getData() { return data; }
    @JsonProperty("data")
    public void setData(Data value) { this.data = value; }

    @JsonProperty("status")
    public Data getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Data value) { this.status = value; }
}
