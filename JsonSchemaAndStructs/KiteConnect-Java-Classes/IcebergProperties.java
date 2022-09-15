package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class IcebergProperties {
    private AveragePrice leg;
    private AveragePrice legQuantity;
    private AveragePrice legs;
    private AveragePrice remainingQuantity;
    private AveragePrice totalQuantity;

    @JsonProperty("leg")
    public AveragePrice getLeg() { return leg; }
    @JsonProperty("leg")
    public void setLeg(AveragePrice value) { this.leg = value; }

    @JsonProperty("leg_quantity")
    public AveragePrice getLegQuantity() { return legQuantity; }
    @JsonProperty("leg_quantity")
    public void setLegQuantity(AveragePrice value) { this.legQuantity = value; }

    @JsonProperty("legs")
    public AveragePrice getLegs() { return legs; }
    @JsonProperty("legs")
    public void setLegs(AveragePrice value) { this.legs = value; }

    @JsonProperty("remaining_quantity")
    public AveragePrice getRemainingQuantity() { return remainingQuantity; }
    @JsonProperty("remaining_quantity")
    public void setRemainingQuantity(AveragePrice value) { this.remainingQuantity = value; }

    @JsonProperty("total_quantity")
    public AveragePrice getTotalQuantity() { return totalQuantity; }
    @JsonProperty("total_quantity")
    public void setTotalQuantity(AveragePrice value) { this.totalQuantity = value; }
}
