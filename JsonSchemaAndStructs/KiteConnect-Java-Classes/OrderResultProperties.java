package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class OrderResultProperties {
    private Exchange orderID;
    private Exchange rejectionReason;
    private Exchange status;

    @JsonProperty("order_id")
    public Exchange getOrderID() { return orderID; }
    @JsonProperty("order_id")
    public void setOrderID(Exchange value) { this.orderID = value; }

    @JsonProperty("rejection_reason")
    public Exchange getRejectionReason() { return rejectionReason; }
    @JsonProperty("rejection_reason")
    public void setRejectionReason(Exchange value) { this.rejectionReason = value; }

    @JsonProperty("status")
    public Exchange getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Exchange value) { this.status = value; }
}
