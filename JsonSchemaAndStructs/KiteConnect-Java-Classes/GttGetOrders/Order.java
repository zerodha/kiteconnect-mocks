package GttGetOrders;

import com.fasterxml.jackson.annotation.*;

public class Order {
    private String exchange;
    private String orderType;
    private Double price;
    private String product;
    private Long quantity;
    private Result result;
    private String tradingsymbol;
    private String transactionType;

    @JsonProperty("exchange")
    public String getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(String value) { this.exchange = value; }

    @JsonProperty("order_type")
    public String getOrderType() { return orderType; }
    @JsonProperty("order_type")
    public void setOrderType(String value) { this.orderType = value; }

    @JsonProperty("price")
    public Double getPrice() { return price; }
    @JsonProperty("price")
    public void setPrice(Double value) { this.price = value; }

    @JsonProperty("product")
    public String getProduct() { return product; }
    @JsonProperty("product")
    public void setProduct(String value) { this.product = value; }

    @JsonProperty("quantity")
    public Long getQuantity() { return quantity; }
    @JsonProperty("quantity")
    public void setQuantity(Long value) { this.quantity = value; }

    @JsonProperty("result")
    public Result getResult() { return result; }
    @JsonProperty("result")
    public void setResult(Result value) { this.result = value; }

    @JsonProperty("tradingsymbol")
    public String getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(String value) { this.tradingsymbol = value; }

    @JsonProperty("transaction_type")
    public String getTransactionType() { return transactionType; }
    @JsonProperty("transaction_type")
    public void setTransactionType(String value) { this.transactionType = value; }
}
