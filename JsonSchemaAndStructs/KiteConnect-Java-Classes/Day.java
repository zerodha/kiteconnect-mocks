package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Day {
    private DataClass items;
    private String type;

    @JsonProperty("items")
    public DataClass getItems() { return items; }
    @JsonProperty("items")
    public void setItems(DataClass value) { this.items = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
