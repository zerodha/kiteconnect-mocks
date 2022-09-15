package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MFSIPInfoProperties {
    private StepUp data;
    private CompletedInstalments status;

    @JsonProperty("data")
    public StepUp getData() { return data; }
    @JsonProperty("data")
    public void setData(StepUp value) { this.data = value; }

    @JsonProperty("status")
    public CompletedInstalments getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(CompletedInstalments value) { this.status = value; }
}
