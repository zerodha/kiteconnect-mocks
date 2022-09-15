package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class AuthorisedQuantity {
    private Type type;

    @JsonProperty("type")
    public Type getType() { return type; }
    @JsonProperty("type")
    public void setType(Type value) { this.type = value; }
}
