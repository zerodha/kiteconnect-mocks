package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class OhlcClassProperties {
    private InstrumentToken close;
    private InstrumentToken high;
    private InstrumentToken low;
    private InstrumentToken open;

    @JsonProperty("close")
    public InstrumentToken getClose() { return close; }
    @JsonProperty("close")
    public void setClose(InstrumentToken value) { this.close = value; }

    @JsonProperty("high")
    public InstrumentToken getHigh() { return high; }
    @JsonProperty("high")
    public void setHigh(InstrumentToken value) { this.high = value; }

    @JsonProperty("low")
    public InstrumentToken getLow() { return low; }
    @JsonProperty("low")
    public void setLow(InstrumentToken value) { this.low = value; }

    @JsonProperty("open")
    public InstrumentToken getOpen() { return open; }
    @JsonProperty("open")
    public void setOpen(InstrumentToken value) { this.open = value; }
}
