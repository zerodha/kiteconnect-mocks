package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Tags {
    private AveragePrice items;
    private String type;

    @JsonProperty("items")
    public AveragePrice getItems() { return items; }
    @JsonProperty("items")
    public void setItems(AveragePrice value) { this.items = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
