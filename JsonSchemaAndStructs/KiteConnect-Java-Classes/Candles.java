package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Candles {
    private Items items;
    private String type;

    @JsonProperty("items")
    public Items getItems() { return items; }
    @JsonProperty("items")
    public void setItems(Items value) { this.items = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
