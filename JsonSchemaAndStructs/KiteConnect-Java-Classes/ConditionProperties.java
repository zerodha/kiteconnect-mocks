package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class ConditionProperties {
    private Exchange exchange;
    private Exchange instrumentToken;
    private Exchange lastPrice;
    private Exchange tradingsymbol;
    private TriggerValues triggerValues;

    @JsonProperty("exchange")
    public Exchange getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(Exchange value) { this.exchange = value; }

    @JsonProperty("instrument_token")
    public Exchange getInstrumentToken() { return instrumentToken; }
    @JsonProperty("instrument_token")
    public void setInstrumentToken(Exchange value) { this.instrumentToken = value; }

    @JsonProperty("last_price")
    public Exchange getLastPrice() { return lastPrice; }
    @JsonProperty("last_price")
    public void setLastPrice(Exchange value) { this.lastPrice = value; }

    @JsonProperty("tradingsymbol")
    public Exchange getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(Exchange value) { this.tradingsymbol = value; }

    @JsonProperty("trigger_values")
    public TriggerValues getTriggerValues() { return triggerValues; }
    @JsonProperty("trigger_values")
    public void setTriggerValues(TriggerValues value) { this.triggerValues = value; }
}
