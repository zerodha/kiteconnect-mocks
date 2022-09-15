package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class ItyProperties {
    private Commodity available;
    private AdhocMargin enabled;
    private AdhocMargin net;
    private Utilised utilised;

    @JsonProperty("available")
    public Commodity getAvailable() { return available; }
    @JsonProperty("available")
    public void setAvailable(Commodity value) { this.available = value; }

    @JsonProperty("enabled")
    public AdhocMargin getEnabled() { return enabled; }
    @JsonProperty("enabled")
    public void setEnabled(AdhocMargin value) { this.enabled = value; }

    @JsonProperty("net")
    public AdhocMargin getNet() { return net; }
    @JsonProperty("net")
    public void setNet(AdhocMargin value) { this.net = value; }

    @JsonProperty("utilised")
    public Utilised getUtilised() { return utilised; }
    @JsonProperty("utilised")
    public void setUtilised(Utilised value) { this.utilised = value; }
}
