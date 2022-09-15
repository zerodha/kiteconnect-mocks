package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class NseInfyProperties {
    private Orders averagePrice;
    private Orders buyQuantity;
    private NseInfy depth;
    private Orders instrumentToken;
    private Orders lastPrice;
    private Orders lastQuantity;
    private LastTradeTime lastTradeTime;
    private Orders lowerCircuitLimit;
    private Orders netChange;
    private NseInfy ohlc;
    private Orders oi;
    private Orders oiDayHigh;
    private Orders oiDayLow;
    private Orders sellQuantity;
    private LastTradeTime timestamp;
    private Orders upperCircuitLimit;
    private Orders volume;

    @JsonProperty("average_price")
    public Orders getAveragePrice() { return averagePrice; }
    @JsonProperty("average_price")
    public void setAveragePrice(Orders value) { this.averagePrice = value; }

    @JsonProperty("buy_quantity")
    public Orders getBuyQuantity() { return buyQuantity; }
    @JsonProperty("buy_quantity")
    public void setBuyQuantity(Orders value) { this.buyQuantity = value; }

    @JsonProperty("depth")
    public NseInfy getDepth() { return depth; }
    @JsonProperty("depth")
    public void setDepth(NseInfy value) { this.depth = value; }

    @JsonProperty("instrument_token")
    public Orders getInstrumentToken() { return instrumentToken; }
    @JsonProperty("instrument_token")
    public void setInstrumentToken(Orders value) { this.instrumentToken = value; }

    @JsonProperty("last_price")
    public Orders getLastPrice() { return lastPrice; }
    @JsonProperty("last_price")
    public void setLastPrice(Orders value) { this.lastPrice = value; }

    @JsonProperty("last_quantity")
    public Orders getLastQuantity() { return lastQuantity; }
    @JsonProperty("last_quantity")
    public void setLastQuantity(Orders value) { this.lastQuantity = value; }

    @JsonProperty("last_trade_time")
    public LastTradeTime getLastTradeTime() { return lastTradeTime; }
    @JsonProperty("last_trade_time")
    public void setLastTradeTime(LastTradeTime value) { this.lastTradeTime = value; }

    @JsonProperty("lower_circuit_limit")
    public Orders getLowerCircuitLimit() { return lowerCircuitLimit; }
    @JsonProperty("lower_circuit_limit")
    public void setLowerCircuitLimit(Orders value) { this.lowerCircuitLimit = value; }

    @JsonProperty("net_change")
    public Orders getNetChange() { return netChange; }
    @JsonProperty("net_change")
    public void setNetChange(Orders value) { this.netChange = value; }

    @JsonProperty("ohlc")
    public NseInfy getOhlc() { return ohlc; }
    @JsonProperty("ohlc")
    public void setOhlc(NseInfy value) { this.ohlc = value; }

    @JsonProperty("oi")
    public Orders getOi() { return oi; }
    @JsonProperty("oi")
    public void setOi(Orders value) { this.oi = value; }

    @JsonProperty("oi_day_high")
    public Orders getOiDayHigh() { return oiDayHigh; }
    @JsonProperty("oi_day_high")
    public void setOiDayHigh(Orders value) { this.oiDayHigh = value; }

    @JsonProperty("oi_day_low")
    public Orders getOiDayLow() { return oiDayLow; }
    @JsonProperty("oi_day_low")
    public void setOiDayLow(Orders value) { this.oiDayLow = value; }

    @JsonProperty("sell_quantity")
    public Orders getSellQuantity() { return sellQuantity; }
    @JsonProperty("sell_quantity")
    public void setSellQuantity(Orders value) { this.sellQuantity = value; }

    @JsonProperty("timestamp")
    public LastTradeTime getTimestamp() { return timestamp; }
    @JsonProperty("timestamp")
    public void setTimestamp(LastTradeTime value) { this.timestamp = value; }

    @JsonProperty("upper_circuit_limit")
    public Orders getUpperCircuitLimit() { return upperCircuitLimit; }
    @JsonProperty("upper_circuit_limit")
    public void setUpperCircuitLimit(Orders value) { this.upperCircuitLimit = value; }

    @JsonProperty("volume")
    public Orders getVolume() { return volume; }
    @JsonProperty("volume")
    public void setVolume(Orders value) { this.volume = value; }
}
