package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class DepthProperties {
    private BuyClass buy;
    private BuyClass sell;

    @JsonProperty("buy")
    public BuyClass getBuy() { return buy; }
    @JsonProperty("buy")
    public void setBuy(BuyClass value) { this.buy = value; }

    @JsonProperty("sell")
    public BuyClass getSell() { return sell; }
    @JsonProperty("sell")
    public void setSell(BuyClass value) { this.sell = value; }
}
