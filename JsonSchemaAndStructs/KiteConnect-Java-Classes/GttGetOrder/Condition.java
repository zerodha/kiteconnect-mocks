package GttGetOrder;

import com.fasterxml.jackson.annotation.*;

public class Condition {
    private String exchange;
    private Long instrumentToken;
    private Double lastPrice;
    private String tradingsymbol;
    private double[] triggerValues;

    @JsonProperty("exchange")
    public String getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(String value) { this.exchange = value; }

    @JsonProperty("instrument_token")
    public Long getInstrumentToken() { return instrumentToken; }
    @JsonProperty("instrument_token")
    public void setInstrumentToken(Long value) { this.instrumentToken = value; }

    @JsonProperty("last_price")
    public Double getLastPrice() { return lastPrice; }
    @JsonProperty("last_price")
    public void setLastPrice(Double value) { this.lastPrice = value; }

    @JsonProperty("tradingsymbol")
    public String getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(String value) { this.tradingsymbol = value; }

    @JsonProperty("trigger_values")
    public double[] getTriggerValues() { return triggerValues; }
    @JsonProperty("trigger_values")
    public void setTriggerValues(double[] value) { this.triggerValues = value; }
}
