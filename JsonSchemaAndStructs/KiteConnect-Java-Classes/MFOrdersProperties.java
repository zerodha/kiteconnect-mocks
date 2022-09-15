package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MFOrdersProperties {
    private Data data;
    private Amount status;

    @JsonProperty("data")
    public Data getData() { return data; }
    @JsonProperty("data")
    public void setData(Data value) { this.data = value; }

    @JsonProperty("status")
    public Amount getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Amount value) { this.status = value; }
}
