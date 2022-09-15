package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class StepUpProperties {
    private CompletedInstalments the1502;

    @JsonProperty("15-02")
    public CompletedInstalments getThe1502() { return the1502; }
    @JsonProperty("15-02")
    public void setThe1502(CompletedInstalments value) { this.the1502 = value; }
}
