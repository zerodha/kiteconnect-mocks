package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class LtpProperties {
    private NseInfy data;
    private Status status;

    @JsonProperty("data")
    public NseInfy getData() { return data; }
    @JsonProperty("data")
    public void setData(NseInfy value) { this.data = value; }

    @JsonProperty("status")
    public Status getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Status value) { this.status = value; }
}
