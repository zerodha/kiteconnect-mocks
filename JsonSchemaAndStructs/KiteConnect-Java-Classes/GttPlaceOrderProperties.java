package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class GttPlaceOrderProperties {
    private DataClass data;
    private TriggerID status;

    @JsonProperty("data")
    public DataClass getData() { return data; }
    @JsonProperty("data")
    public void setData(DataClass value) { this.data = value; }

    @JsonProperty("status")
    public TriggerID getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(TriggerID value) { this.status = value; }
}
