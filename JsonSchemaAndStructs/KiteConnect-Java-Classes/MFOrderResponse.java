package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MFOrderResponse {
    private String ref;
    private String schema;
    private Definitions definitions;

    @JsonProperty("$ref")
    public String getRef() { return ref; }
    @JsonProperty("$ref")
    public void setRef(String value) { this.ref = value; }

    @JsonProperty("$schema")
    public String getSchema() { return schema; }
    @JsonProperty("$schema")
    public void setSchema(String value) { this.schema = value; }

    @JsonProperty("definitions")
    public Definitions getDefinitions() { return definitions; }
    @JsonProperty("definitions")
    public void setDefinitions(Definitions value) { this.definitions = value; }
}
