package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class OrderCancelProperties {
    private DataClass data;
    private OrderID status;

    @JsonProperty("data")
    public DataClass getData() { return data; }
    @JsonProperty("data")
    public void setData(DataClass value) { this.data = value; }

    @JsonProperty("status")
    public OrderID getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(OrderID value) { this.status = value; }
}
