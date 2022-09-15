package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class ResultProperties {
    private Exchange accountID;
    private Exchange exchange;
    private Exchange meta;
    private ConditionClass orderResult;
    private Exchange orderType;
    private Exchange price;
    private Exchange product;
    private Exchange quantity;
    private CreatedAt timestamp;
    private Exchange tradingsymbol;
    private Exchange transactionType;
    private Exchange triggeredAt;
    private Exchange validity;

    @JsonProperty("account_id")
    public Exchange getAccountID() { return accountID; }
    @JsonProperty("account_id")
    public void setAccountID(Exchange value) { this.accountID = value; }

    @JsonProperty("exchange")
    public Exchange getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(Exchange value) { this.exchange = value; }

    @JsonProperty("meta")
    public Exchange getMeta() { return meta; }
    @JsonProperty("meta")
    public void setMeta(Exchange value) { this.meta = value; }

    @JsonProperty("order_result")
    public ConditionClass getOrderResult() { return orderResult; }
    @JsonProperty("order_result")
    public void setOrderResult(ConditionClass value) { this.orderResult = value; }

    @JsonProperty("order_type")
    public Exchange getOrderType() { return orderType; }
    @JsonProperty("order_type")
    public void setOrderType(Exchange value) { this.orderType = value; }

    @JsonProperty("price")
    public Exchange getPrice() { return price; }
    @JsonProperty("price")
    public void setPrice(Exchange value) { this.price = value; }

    @JsonProperty("product")
    public Exchange getProduct() { return product; }
    @JsonProperty("product")
    public void setProduct(Exchange value) { this.product = value; }

    @JsonProperty("quantity")
    public Exchange getQuantity() { return quantity; }
    @JsonProperty("quantity")
    public void setQuantity(Exchange value) { this.quantity = value; }

    @JsonProperty("timestamp")
    public CreatedAt getTimestamp() { return timestamp; }
    @JsonProperty("timestamp")
    public void setTimestamp(CreatedAt value) { this.timestamp = value; }

    @JsonProperty("tradingsymbol")
    public Exchange getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(Exchange value) { this.tradingsymbol = value; }

    @JsonProperty("transaction_type")
    public Exchange getTransactionType() { return transactionType; }
    @JsonProperty("transaction_type")
    public void setTransactionType(Exchange value) { this.transactionType = value; }

    @JsonProperty("triggered_at")
    public Exchange getTriggeredAt() { return triggeredAt; }
    @JsonProperty("triggered_at")
    public void setTriggeredAt(Exchange value) { this.triggeredAt = value; }

    @JsonProperty("validity")
    public Exchange getValidity() { return validity; }
    @JsonProperty("validity")
    public void setValidity(Exchange value) { this.validity = value; }
}
