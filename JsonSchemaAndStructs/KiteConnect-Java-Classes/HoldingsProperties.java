package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class HoldingsProperties {
    private Data data;
    private AuthorisedQuantity status;

    @JsonProperty("data")
    public Data getData() { return data; }
    @JsonProperty("data")
    public void setData(Data value) { this.data = value; }

    @JsonProperty("status")
    public AuthorisedQuantity getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(AuthorisedQuantity value) { this.status = value; }
}
