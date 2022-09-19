package GttGetOrders;

import com.fasterxml.jackson.annotation.*;
import java.time.OffsetDateTime;

public class Datum {
    private Condition condition;
    private OffsetDateTime createdAt;
    private OffsetDateTime expiresAt;
    private Long id;
    private Meta meta;
    private Order[] orders;
    private Object parentTrigger;
    private String status;
    private String type;
    private OffsetDateTime updatedAt;
    private String userID;

    @JsonProperty("condition")
    public Condition getCondition() { return condition; }
    @JsonProperty("condition")
    public void setCondition(Condition value) { this.condition = value; }

    @JsonProperty("created_at")
    public OffsetDateTime getCreatedAt() { return createdAt; }
    @JsonProperty("created_at")
    public void setCreatedAt(OffsetDateTime value) { this.createdAt = value; }

    @JsonProperty("expires_at")
    public OffsetDateTime getExpiresAt() { return expiresAt; }
    @JsonProperty("expires_at")
    public void setExpiresAt(OffsetDateTime value) { this.expiresAt = value; }

    @JsonProperty("id")
    public Long getID() { return id; }
    @JsonProperty("id")
    public void setID(Long value) { this.id = value; }

    @JsonProperty("meta")
    public Meta getMeta() { return meta; }
    @JsonProperty("meta")
    public void setMeta(Meta value) { this.meta = value; }

    @JsonProperty("orders")
    public Order[] getOrders() { return orders; }
    @JsonProperty("orders")
    public void setOrders(Order[] value) { this.orders = value; }

    @JsonProperty("parent_trigger")
    public Object getParentTrigger() { return parentTrigger; }
    @JsonProperty("parent_trigger")
    public void setParentTrigger(Object value) { this.parentTrigger = value; }

    @JsonProperty("status")
    public String getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(String value) { this.status = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }

    @JsonProperty("updated_at")
    public OffsetDateTime getUpdatedAt() { return updatedAt; }
    @JsonProperty("updated_at")
    public void setUpdatedAt(OffsetDateTime value) { this.updatedAt = value; }

    @JsonProperty("user_id")
    public String getUserID() { return userID; }
    @JsonProperty("user_id")
    public void setUserID(String value) { this.userID = value; }
}
