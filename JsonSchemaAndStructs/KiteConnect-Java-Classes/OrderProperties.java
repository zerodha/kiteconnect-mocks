package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class OrderProperties {
    private Exchange exchange;
    private Exchange orderType;
    private Exchange price;
    private Exchange product;
    private Exchange quantity;
    private Meta result;
    private Exchange tradingsymbol;
    private Exchange transactionType;

    @JsonProperty("exchange")
    public Exchange getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(Exchange value) { this.exchange = value; }

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

    @JsonProperty("result")
    public Meta getResult() { return result; }
    @JsonProperty("result")
    public void setResult(Meta value) { this.result = value; }

    @JsonProperty("tradingsymbol")
    public Exchange getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(Exchange value) { this.tradingsymbol = value; }

    @JsonProperty("transaction_type")
    public Exchange getTransactionType() { return transactionType; }
    @JsonProperty("transaction_type")
    public void setTransactionType(Exchange value) { this.transactionType = value; }
}
