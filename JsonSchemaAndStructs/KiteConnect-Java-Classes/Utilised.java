package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Utilised {
    private AdhocMargin additionalProperties;
    private String type;

    @JsonProperty("additionalProperties")
    public AdhocMargin getAdditionalProperties() { return additionalProperties; }
    @JsonProperty("additionalProperties")
    public void setAdditionalProperties(AdhocMargin value) { this.additionalProperties = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
