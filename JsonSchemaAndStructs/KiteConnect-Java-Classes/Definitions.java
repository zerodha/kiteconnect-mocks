package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Definitions {
    private Data data;
    private Nse nse;
    private TriggerRangeClass triggerRange;

    @JsonProperty("Data")
    public Data getData() { return data; }
    @JsonProperty("Data")
    public void setData(Data value) { this.data = value; }

    @JsonProperty("Nse")
    public Nse getNse() { return nse; }
    @JsonProperty("Nse")
    public void setNse(Nse value) { this.nse = value; }

    @JsonProperty("TriggerRange")
    public TriggerRangeClass getTriggerRange() { return triggerRange; }
    @JsonProperty("TriggerRange")
    public void setTriggerRange(TriggerRangeClass value) { this.triggerRange = value; }
}
