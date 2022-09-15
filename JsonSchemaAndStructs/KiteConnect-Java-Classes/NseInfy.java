package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class NseInfy {
    private String ref;

    @JsonProperty("$ref")
    public String getRef() { return ref; }
    @JsonProperty("$ref")
    public void setRef(String value) { this.ref = value; }
}
