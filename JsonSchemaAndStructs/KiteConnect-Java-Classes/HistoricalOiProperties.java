package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class HistoricalOiProperties {
    private DataClass data;
    private AnyOf status;

    @JsonProperty("data")
    public DataClass getData() { return data; }
    @JsonProperty("data")
    public void setData(DataClass value) { this.data = value; }

    @JsonProperty("status")
    public AnyOf getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(AnyOf value) { this.status = value; }
}
