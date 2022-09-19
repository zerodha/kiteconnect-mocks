package HistoricalMinute;

import com.fasterxml.jackson.annotation.*;

public class Data {
    private Candle[][] candles;

    @JsonProperty("candles")
    public Candle[][] getCandles() { return candles; }
    @JsonProperty("candles")
    public void setCandles(Candle[][] value) { this.candles = value; }
}
