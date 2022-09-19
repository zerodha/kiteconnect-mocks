package GttGetOrders;

import com.fasterxml.jackson.annotation.*;

public class OrderResult {
    private String orderID;
    private String rejectionReason;
    private String status;

    @JsonProperty("order_id")
    public String getOrderID() { return orderID; }
    @JsonProperty("order_id")
    public void setOrderID(String value) { this.orderID = value; }

    @JsonProperty("rejection_reason")
    public String getRejectionReason() { return rejectionReason; }
    @JsonProperty("rejection_reason")
    public void setRejectionReason(String value) { this.rejectionReason = value; }

    @JsonProperty("status")
    public String getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(String value) { this.status = value; }
}
