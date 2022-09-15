package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class FinalProperties {
    private Status additional;
    private Status bo;
    private Status cash;
    private Status exchange;
    private Status exposure;
    private Status optionPremium;
    private Data pnl;
    private Status span;
    private Status total;
    private Status tradingsymbol;
    private Status type;
    private Status var;

    @JsonProperty("additional")
    public Status getAdditional() { return additional; }
    @JsonProperty("additional")
    public void setAdditional(Status value) { this.additional = value; }

    @JsonProperty("bo")
    public Status getBo() { return bo; }
    @JsonProperty("bo")
    public void setBo(Status value) { this.bo = value; }

    @JsonProperty("cash")
    public Status getCash() { return cash; }
    @JsonProperty("cash")
    public void setCash(Status value) { this.cash = value; }

    @JsonProperty("exchange")
    public Status getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(Status value) { this.exchange = value; }

    @JsonProperty("exposure")
    public Status getExposure() { return exposure; }
    @JsonProperty("exposure")
    public void setExposure(Status value) { this.exposure = value; }

    @JsonProperty("option_premium")
    public Status getOptionPremium() { return optionPremium; }
    @JsonProperty("option_premium")
    public void setOptionPremium(Status value) { this.optionPremium = value; }

    @JsonProperty("pnl")
    public Data getPnl() { return pnl; }
    @JsonProperty("pnl")
    public void setPnl(Data value) { this.pnl = value; }

    @JsonProperty("span")
    public Status getSpan() { return span; }
    @JsonProperty("span")
    public void setSpan(Status value) { this.span = value; }

    @JsonProperty("total")
    public Status getTotal() { return total; }
    @JsonProperty("total")
    public void setTotal(Status value) { this.total = value; }

    @JsonProperty("tradingsymbol")
    public Status getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(Status value) { this.tradingsymbol = value; }

    @JsonProperty("type")
    public Status getType() { return type; }
    @JsonProperty("type")
    public void setType(Status value) { this.type = value; }

    @JsonProperty("var")
    public Status getVar() { return var; }
    @JsonProperty("var")
    public void setVar(Status value) { this.var = value; }
}
