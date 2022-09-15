package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class OhlcProperties {
    private Orders close;
    private Orders high;
    private Orders low;
    private Orders open;

    @JsonProperty("close")
    public Orders getClose() { return close; }
    @JsonProperty("close")
    public void setClose(Orders value) { this.close = value; }

    @JsonProperty("high")
    public Orders getHigh() { return high; }
    @JsonProperty("high")
    public void setHigh(Orders value) { this.high = value; }

    @JsonProperty("low")
    public Orders getLow() { return low; }
    @JsonProperty("low")
    public void setLow(Orders value) { this.low = value; }

    @JsonProperty("open")
    public Orders getOpen() { return open; }
    @JsonProperty("open")
    public void setOpen(Orders value) { this.open = value; }
}
