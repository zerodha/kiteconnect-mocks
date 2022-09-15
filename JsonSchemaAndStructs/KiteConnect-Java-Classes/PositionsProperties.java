package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class PositionsProperties {
    private DataClass data;
    private Property status;

    @JsonProperty("data")
    public DataClass getData() { return data; }
    @JsonProperty("data")
    public void setData(DataClass value) { this.data = value; }

    @JsonProperty("status")
    public Property getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Property value) { this.status = value; }
}
