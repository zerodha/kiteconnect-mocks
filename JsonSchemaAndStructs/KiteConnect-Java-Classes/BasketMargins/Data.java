package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Data {
    private Final dataFinal;
    private Final initial;
    private Final[] orders;

    @JsonProperty("final")
    public Final getDataFinal() { return dataFinal; }
    @JsonProperty("final")
    public void setDataFinal(Final value) { this.dataFinal = value; }

    @JsonProperty("initial")
    public Final getInitial() { return initial; }
    @JsonProperty("initial")
    public void setInitial(Final value) { this.initial = value; }

    @JsonProperty("orders")
    public Final[] getOrders() { return orders; }
    @JsonProperty("orders")
    public void setOrders(Final[] value) { this.orders = value; }
}
