package Holdings;

import com.fasterxml.jackson.annotation.*;

public class Holdings {
    private Datum[] data;
    private String status;

    @JsonProperty("data")
    public Datum[] getData() { return data; }
    @JsonProperty("data")
    public void setData(Datum[] value) { this.data = value; }

    @JsonProperty("status")
    public String getStatus() { return status; }
    @JsonProperty("status")
    public void setStatus(String value) { this.status = value; }
}
