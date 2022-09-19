package GttGetOrder;

import com.fasterxml.jackson.annotation.*;
import java.time.OffsetDateTime;

public class Result {
    private String accountID;
    private String exchange;
    private String meta;
    private OrderResult orderResult;
    private String orderType;
    private Long price;
    private String product;
    private Long quantity;
    private OffsetDateTime timestamp;
    private String tradingsymbol;
    private String transactionType;
    private Double triggeredAt;
    private String validity;

    @JsonProperty("account_id")
    public String getAccountID() { return accountID; }
    @JsonProperty("account_id")
    public void setAccountID(String value) { this.accountID = value; }

    @JsonProperty("exchange")
    public String getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(String value) { this.exchange = value; }

    @JsonProperty("meta")
    public String getMeta() { return meta; }
    @JsonProperty("meta")
    public void setMeta(String value) { this.meta = value; }

    @JsonProperty("order_result")
    public OrderResult getOrderResult() { return orderResult; }
    @JsonProperty("order_result")
    public void setOrderResult(OrderResult value) { this.orderResult = value; }

    @JsonProperty("order_type")
    public String getOrderType() { return orderType; }
    @JsonProperty("order_type")
    public void setOrderType(String value) { this.orderType = value; }

    @JsonProperty("price")
    public Long getPrice() { return price; }
    @JsonProperty("price")
    public void setPrice(Long value) { this.price = value; }

    @JsonProperty("product")
    public String getProduct() { return product; }
    @JsonProperty("product")
    public void setProduct(String value) { this.product = value; }

    @JsonProperty("quantity")
    public Long getQuantity() { return quantity; }
    @JsonProperty("quantity")
    public void setQuantity(Long value) { this.quantity = value; }

    @JsonProperty("timestamp")
    public OffsetDateTime getTimestamp() { return timestamp; }
    @JsonProperty("timestamp")
    public void setTimestamp(OffsetDateTime value) { this.timestamp = value; }

    @JsonProperty("tradingsymbol")
    public String getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(String value) { this.tradingsymbol = value; }

    @JsonProperty("transaction_type")
    public String getTransactionType() { return transactionType; }
    @JsonProperty("transaction_type")
    public void setTransactionType(String value) { this.transactionType = value; }

    @JsonProperty("triggered_at")
    public Double getTriggeredAt() { return triggeredAt; }
    @JsonProperty("triggered_at")
    public void setTriggeredAt(Double value) { this.triggeredAt = value; }

    @JsonProperty("validity")
    public String getValidity() { return validity; }
    @JsonProperty("validity")
    public void setValidity(String value) { this.validity = value; }
}
