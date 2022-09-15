package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Exchanges {
    private AvatarURL items;
    private String type;

    @JsonProperty("items")
    public AvatarURL getItems() { return items; }
    @JsonProperty("items")
    public void setItems(AvatarURL value) { this.items = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
