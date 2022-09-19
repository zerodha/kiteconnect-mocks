package ConvertPosition;

import com.fasterxml.jackson.annotation.*;

public class ConvertPosition {
    private Boolean data;
    private String status;

    @JsonProperty("data")
    public Boolean getData() { return data; }
    @JsonProperty("data")
    public void setData(Boolean value) { this.data = value; }

    @JsonProperty("status")
    public String getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(String value) { this.status = value; }
}
