package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class TradesProperties {
    private Data data;
    private AveragePrice status;

    @JsonProperty("data")
    public Data getData() { return data; }
    @JsonProperty("data")
    public void setData(Data value) { this.data = value; }

    @JsonProperty("status")
    public AveragePrice getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(AveragePrice value) { this.status = value; }
}
