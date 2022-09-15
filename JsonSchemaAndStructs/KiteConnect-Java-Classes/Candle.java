package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class Candle {
    private AnyOf[] anyOf;
    private String title;

    @JsonProperty("anyOf")
    public AnyOf[] getAnyOf() { return anyOf; }
    @JsonProperty("anyOf")
    public void setAnyOf(AnyOf[] value) { this.anyOf = value; }

    @JsonProperty("title")
    public String getTitle() { return title; }
    @JsonProperty("title")
    public void setTitle(String value) { this.title = value; }
}
