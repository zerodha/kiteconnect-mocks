package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class GttGetOrderProperties {
    private ConditionClass data;
    private Exchange status;

    @JsonProperty("data")
    public ConditionClass getData() { return data; }
    @JsonProperty("data")
    public void setData(ConditionClass value) { this.data = value; }

    @JsonProperty("status")
    public Exchange getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Exchange value) { this.status = value; }
}
