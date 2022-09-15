package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class PnlProperties {
    private Additional realised;
    private Additional unrealised;

    @JsonProperty("realised")
    public Additional getRealised() { return realised; }
    @JsonProperty("realised")
    public void setRealised(Additional value) { this.realised = value; }

    @JsonProperty("unrealised")
    public Additional getUnrealised() { return unrealised; }
    @JsonProperty("unrealised")
    public void setUnrealised(Additional value) { this.unrealised = value; }
}
