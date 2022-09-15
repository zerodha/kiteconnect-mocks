package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class OrderMarginsClass {
    private boolean additionalProperties;
    private OrderMarginsProperties properties;
    private String[] required;
    private String title;
    private String type;

    @JsonProperty("additionalProperties")
    public boolean getAdditionalProperties() { return additionalProperties; }
    @JsonProperty("additionalProperties")
    public void setAdditionalProperties(boolean value) { this.additionalProperties = value; }

    @JsonProperty("properties")
    public OrderMarginsProperties getProperties() { return properties; }
    @JsonProperty("properties")
    public void setProperties(OrderMarginsProperties value) { this.properties = value; }

    @JsonProperty("required")
    public String[] getRequired() { return required; }
    @JsonProperty("required")
    public void setRequired(String[] value) { this.required = value; }

    @JsonProperty("title")
    public String getTitle() { return title; }
    @JsonProperty("title")
    public void setTitle(String value) { this.title = value; }

    @JsonProperty("type")
    public String getType() { return type; }
    @JsonProperty("type")
    public void setType(String value) { this.type = value; }
}
