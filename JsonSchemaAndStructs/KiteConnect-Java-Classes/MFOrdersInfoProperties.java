package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MFOrdersInfoProperties {
    private DataClass data;
    private Amount status;

    @JsonProperty("data")
    public DataClass getData() { return data; }
    @JsonProperty("data")
    public void setData(DataClass value) { this.data = value; }

    @JsonProperty("status")
    public Amount getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Amount value) { this.status = value; }
}
