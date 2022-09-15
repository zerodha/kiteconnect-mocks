package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MFSIPCancelProperties {
    private DataClass data;
    private Sipid status;

    @JsonProperty("data")
    public DataClass getData() { return data; }
    @JsonProperty("data")
    public void setData(DataClass value) { this.data = value; }

    @JsonProperty("status")
    public Sipid getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(Sipid value) { this.status = value; }
}
