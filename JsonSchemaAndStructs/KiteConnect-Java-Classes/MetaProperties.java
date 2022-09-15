package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class MetaProperties {
    private AvatarURL dematConsent;

    @JsonProperty("demat_consent")
    public AvatarURL getDematConsent() { return dematConsent; }
    @JsonProperty("demat_consent")
    public void setDematConsent(AvatarURL value) { this.dematConsent = value; }
}
