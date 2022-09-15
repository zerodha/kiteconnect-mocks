package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MarginsEquityProperties {
    private AvailableClass data;
    private AdhocMargin status;

    @JsonProperty("data")
    public AvailableClass getData() { return data; }
    @JsonProperty("data")
    public void setData(AvailableClass value) { this.data = value; }

    @JsonProperty("status")
    public AdhocMargin getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(AdhocMargin value) { this.status = value; }
}
