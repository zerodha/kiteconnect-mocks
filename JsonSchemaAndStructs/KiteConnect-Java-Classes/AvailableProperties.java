package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class AvailableProperties {
    private AdhocMargin adhocMargin;
    private AdhocMargin cash;
    private AdhocMargin collateral;
    private AdhocMargin intradayPayin;
    private AdhocMargin liveBalance;
    private AdhocMargin openingBalance;

    @JsonProperty("adhoc_margin")
    public AdhocMargin getAdhocMargin() { return adhocMargin; }
    @JsonProperty("adhoc_margin")
    public void setAdhocMargin(AdhocMargin value) { this.adhocMargin = value; }

    @JsonProperty("cash")
    public AdhocMargin getCash() { return cash; }
    @JsonProperty("cash")
    public void setCash(AdhocMargin value) { this.cash = value; }

    @JsonProperty("collateral")
    public AdhocMargin getCollateral() { return collateral; }
    @JsonProperty("collateral")
    public void setCollateral(AdhocMargin value) { this.collateral = value; }

    @JsonProperty("intraday_payin")
    public AdhocMargin getIntradayPayin() { return intradayPayin; }
    @JsonProperty("intraday_payin")
    public void setIntradayPayin(AdhocMargin value) { this.intradayPayin = value; }

    @JsonProperty("live_balance")
    public AdhocMargin getLiveBalance() { return liveBalance; }
    @JsonProperty("live_balance")
    public void setLiveBalance(AdhocMargin value) { this.liveBalance = value; }

    @JsonProperty("opening_balance")
    public AdhocMargin getOpeningBalance() { return openingBalance; }
    @JsonProperty("opening_balance")
    public void setOpeningBalance(AdhocMargin value) { this.openingBalance = value; }
}
