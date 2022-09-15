package io.quicktype;

import com.fasterxml.jackson.annotation.*;

public class DatumProperties {
    private AveragePrice averagePrice;
    private AveragePrice exchange;
    private AveragePrice exchangeOrderID;
    private ExchangeTimestamp exchangeTimestamp;
    private ExchangeTimestamp fillTimestamp;
    private AveragePrice instrumentToken;
    private AveragePrice orderID;
    private ExchangeTimestamp orderTimestamp;
    private AveragePrice product;
    private AveragePrice quantity;
    private ExchangeTimestamp tradeID;
    private AveragePrice tradingsymbol;
    private AveragePrice transactionType;

    @JsonProperty("average_price")
    public AveragePrice getAveragePrice() { return averagePrice; }
    @JsonProperty("average_price")
    public void setAveragePrice(AveragePrice value) { this.averagePrice = value; }

    @JsonProperty("exchange")
    public AveragePrice getExchange() { return exchange; }
    @JsonProperty("exchange")
    public void setExchange(AveragePrice value) { this.exchange = value; }

    @JsonProperty("exchange_order_id")
    public AveragePrice getExchangeOrderID() { return exchangeOrderID; }
    @JsonProperty("exchange_order_id")
    public void setExchangeOrderID(AveragePrice value) { this.exchangeOrderID = value; }

    @JsonProperty("exchange_timestamp")
    public ExchangeTimestamp getExchangeTimestamp() { return exchangeTimestamp; }
    @JsonProperty("exchange_timestamp")
    public void setExchangeTimestamp(ExchangeTimestamp value) { this.exchangeTimestamp = value; }

    @JsonProperty("fill_timestamp")
    public ExchangeTimestamp getFillTimestamp() { return fillTimestamp; }
    @JsonProperty("fill_timestamp")
    public void setFillTimestamp(ExchangeTimestamp value) { this.fillTimestamp = value; }

    @JsonProperty("instrument_token")
    public AveragePrice getInstrumentToken() { return instrumentToken; }
    @JsonProperty("instrument_token")
    public void setInstrumentToken(AveragePrice value) { this.instrumentToken = value; }

    @JsonProperty("order_id")
    public AveragePrice getOrderID() { return orderID; }
    @JsonProperty("order_id")
    public void setOrderID(AveragePrice value) { this.orderID = value; }

    @JsonProperty("order_timestamp")
    public ExchangeTimestamp getOrderTimestamp() { return orderTimestamp; }
    @JsonProperty("order_timestamp")
    public void setOrderTimestamp(ExchangeTimestamp value) { this.orderTimestamp = value; }

    @JsonProperty("product")
    public AveragePrice getProduct() { return product; }
    @JsonProperty("product")
    public void setProduct(AveragePrice value) { this.product = value; }

    @JsonProperty("quantity")
    public AveragePrice getQuantity() { return quantity; }
    @JsonProperty("quantity")
    public void setQuantity(AveragePrice value) { this.quantity = value; }

    @JsonProperty("trade_id")
    public ExchangeTimestamp getTradeID() { return tradeID; }
    @JsonProperty("trade_id")
    public void setTradeID(ExchangeTimestamp value) { this.tradeID = value; }

    @JsonProperty("tradingsymbol")
    public AveragePrice getTradingsymbol() { return tradingsymbol; }
    @JsonProperty("tradingsymbol")
    public void setTradingsymbol(AveragePrice value) { this.tradingsymbol = value; }

    @JsonProperty("transaction_type")
    public AveragePrice getTransactionType() { return transactionType; }
    @JsonProperty("transaction_type")
    public void setTransactionType(AveragePrice value) { this.transactionType = value; }
}
