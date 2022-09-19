package Holdings;

import com.fasterxml.jackson.annotation.*;
import java.time.OffsetDateTime;

public class Datum {
    private OffsetDateTime authorisedDate;
    private Long authorisedQuantity;
    private Double averagePrice;
    private Double closePrice;
    private Long collateralQuantity;
    private String collateralType;
    private Double dayChange;
    private Double dayChangePercentage;
    private Boolean discrepancy;
    private String exchange;
    private Long instrumentToken;
    private String isin;
    private Double lastPrice;
    private Long openingQuantity;
    private Double pnl;
    private Long price;
    private String product;
    private Long quantity;
    private Long realisedQuantity;
    private Long t1Quantity;
    private String tradingsymbol;
    private Long usedQuantity;

    @JsonProperty("authorised_date")
    public OffsetDateTime getAuthorisedDate() { return authorisedDate; }
    @JsonProperty("authorised_date")
    public void setAuthorisedDate(OffsetDateTime value) { this.authorisedDate = value; }

    @JsonProperty("authorised_quantity")
    public Long getAuthorisedQuantity() { return authorisedQuantity; }
    @JsonProperty("authorised_quantity")
    public void setAuthorisedQuantity(Long value) { this.authorisedQuantity = value; }

    @JsonProperty("average_price")
    public Double getAveragePrice() { return averagePrice; }
    @JsonProperty("average_price")
    public void setAveragePrice(Double value) { this.averagePrice = value; }

    @JsonProperty("close_price")
    public Double getClosePrice() { return closePrice; }
    @JsonProperty("close_price")
    public void setClosePrice(Double value) { this.closePrice = value; }

    @JsonProperty("collateral_quantity")
    public Long getCollateralQuantity() { return collateralQuantity; }
    @JsonProperty("collateral_quantity")
    public void setCollateralQuantity(Long value) { this.collateralQuantity = value; }

    @JsonProperty("collateral_type")
    public String getCollateralType() { return collateralType; }
    @JsonProperty("collateral_type")
    public void setCollateralType(String value) { this.collateralType = value; }

    @JsonProperty("day_change")
    public Double getDayChange() { return dayChange; }
    @JsonProperty("day_change")
    public void setDayChange(Double value) { this.dayChange = value; }

    @JsonProperty("day_change_percentage")
    public Double getDayChangePercentage() { return dayChangePercentage; }
    @JsonProperty("day_change_percentage")
    public void setDayChangePercentage(Double value) { this.dayChangePercentage = value; }

    @JsonProperty("discrepancy")
    public Boolean getDiscrepancy() { return discrepancy; }
    @JsonProperty("discrepancy")
    public void setDiscrepancy(Boolean value) { this.discrepancy = value; }

    @JsonProperty("exchange")
    public String getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(String value) { this.exchange = value; }

    @JsonProperty("instrument_token")
    public Long getInstrumentToken() { return instrumentToken; }
    @JsonProperty("instrument_token")
    public void setInstrumentToken(Long value) { this.instrumentToken = value; }

    @JsonProperty("isin")
    public String getIsin() { return isin; }
    @JsonProperty("isin")
    public void setIsin(String value) { this.isin = value; }

    @JsonProperty("last_price")
    public Double getLastPrice() { return lastPrice; }
    @JsonProperty("last_price")
    public void setLastPrice(Double value) { this.lastPrice = value; }

    @JsonProperty("opening_quantity")
    public Long getOpeningQuantity() { return openingQuantity; }
    @JsonProperty("opening_quantity")
    public void setOpeningQuantity(Long value) { this.openingQuantity = value; }

    @JsonProperty("pnl")
    public Double getPnl() { return pnl; }
    @JsonProperty("pnl")
    public void setPnl(Double value) { this.pnl = value; }

    @JsonProperty("price")
    public Long getPrice() { return price; }
    @JsonProperty("price")
    public void setPrice(Long value) { this.price = value; }

    @JsonProperty("product")
    public String getProduct() { return product; }
    @JsonProperty("product")
    public void setProduct(String value) { this.product = value; }

    @JsonProperty("quantity")
    public Long getQuantity() { return quantity; }
    @JsonProperty("quantity")
    public void setQuantity(Long value) { this.quantity = value; }

    @JsonProperty("realised_quantity")
    public Long getRealisedQuantity() { return realisedQuantity; }
    @JsonProperty("realised_quantity")
    public void setRealisedQuantity(Long value) { this.realisedQuantity = value; }

    @JsonProperty("t1_quantity")
    public Long getT1Quantity() { return t1Quantity; }
    @JsonProperty("t1_quantity")
    public void setT1Quantity(Long value) { this.t1Quantity = value; }

    @JsonProperty("tradingsymbol")
    public String getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(String value) { this.tradingsymbol = value; }

    @JsonProperty("used_quantity")
    public Long getUsedQuantity() { return usedQuantity; }
    @JsonProperty("used_quantity")
    public void setUsedQuantity(Long value) { this.usedQuantity = value; }
}
