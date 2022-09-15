package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class BuyClass {
    private NseInfy items;
    private String type;

    @JsonProperty("items")
    public NseInfy getItems() { return items; }
    @JsonProperty("items")
    public void setItems(NseInfy value) { this.items = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
