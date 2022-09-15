package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class ProfileProperties {
    private Meta data;
    private AvatarURL status;

    @JsonProperty("data")
    public Meta getData() { return data; }
    @JsonProperty("data")
    public void setData(Meta value) { this.data = value; }

    @JsonProperty("status")
    public AvatarURL getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(AvatarURL value) { this.status = value; }
}
