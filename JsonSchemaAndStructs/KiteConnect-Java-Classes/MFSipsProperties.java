package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MFSipsProperties {
    private Data data;

    @JsonProperty("data")
    public Data getData() { return data; }
    @JsonProperty("data")
    public void setData(Data value) { this.data = value; }
}
