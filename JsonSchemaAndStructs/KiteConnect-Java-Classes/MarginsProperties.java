package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MarginsProperties {
    private Commodity data;
    private AdhocMargin status;

    @JsonProperty("data")
    public Commodity getData() { return data; }
    @JsonProperty("data")
    public void setData(Commodity value) { this.data = value; }

    @JsonProperty("status")
    public AdhocMargin getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(AdhocMargin value) { this.status = value; }
}
