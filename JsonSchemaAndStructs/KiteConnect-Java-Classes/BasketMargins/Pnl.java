package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Pnl {
    private Long realised;
    private Long unrealised;

    @JsonProperty("realised")
    public Long getRealised() { return realised; }
    @JsonProperty("realised")
    public void setRealised(Long value) { this.realised = value; }

    @JsonProperty("unrealised")
    public Long getUnrealised() { return unrealised; }
    @JsonProperty("unrealised")
    public void setUnrealised(Long value) { this.unrealised = value; }
}
