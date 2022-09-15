package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class StepUp {
    private CompletedInstalments additionalProperties;
    private String type;

    @JsonProperty("additionalProperties")
    public CompletedInstalments getAdditionalProperties() { return additionalProperties; }
    @JsonProperty("additionalProperties")
    public void setAdditionalProperties(CompletedInstalments value) { this.additionalProperties = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
