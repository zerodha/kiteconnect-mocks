package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MFOrderCancelProperties {
    private DataClass data;
    private Status status;

    @JsonProperty("data")
    public DataClass getData() { return data; }
    @JsonProperty("data")
    public void setData(DataClass value) { this.data = value; }

    @JsonProperty("status")
    public Status getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Status value) { this.status = value; }
}
