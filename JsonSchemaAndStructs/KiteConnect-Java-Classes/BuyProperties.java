package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class BuyProperties {
    private Orders orders;
    private Orders price;
    private Orders quantity;

    @JsonProperty("orders")
    public Orders getOrders() { return orders; }
    @JsonProperty("orders")
    public void setOrders(Orders value) { this.orders = value; }

    @JsonProperty("price")
    public Orders getPrice() { return price; }
    @JsonProperty("price")
    public void setPrice(Orders value) { this.price = value; }

    @JsonProperty("quantity")
    public Orders getQuantity() { return quantity; }
    @JsonProperty("quantity")
    public void setQuantity(Orders value) { this.quantity = value; }
}
