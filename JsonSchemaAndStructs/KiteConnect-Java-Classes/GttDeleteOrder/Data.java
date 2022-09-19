package GttDeleteOrder;

import com.fasterxml.jackson.annotation.*;

public class Data {
    private Long triggerID;

    @JsonProperty("trigger_id")
    public Long getTriggerID() { return triggerID; }
    @JsonProperty("trigger_id")
    public void setTriggerID(Long value) { this.triggerID = value; }
}
