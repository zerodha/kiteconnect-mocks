package GenerateSession;

import com.fasterxml.jackson.annotation.*;

public class Meta {
    private String dematConsent;

    @JsonProperty("demat_consent")
    public String getDematConsent() { return dematConsent; }
    @JsonProperty("demat_consent")
    public void setDematConsent(String value) { this.dematConsent = value; }
}
