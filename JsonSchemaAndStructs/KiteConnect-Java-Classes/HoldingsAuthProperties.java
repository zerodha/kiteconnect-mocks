package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class HoldingsAuthProperties {
    private DataClass data;
    private RequestID status;

    @JsonProperty("data")
    public DataClass getData() { return data; }
    @JsonProperty("data")
    public void setData(DataClass value) { this.data = value; }

    @JsonProperty("status")
    public RequestID getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(RequestID value) { this.status = value; }
}
