package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class TriggerValues {
    private Exchange items;
    private String type;

    @JsonProperty("items")
    public Exchange getItems() { return items; }
    @JsonProperty("items")
    public void setItems(Exchange value) { this.items = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
