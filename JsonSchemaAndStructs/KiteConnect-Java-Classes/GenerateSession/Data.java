package GenerateSession;

import com.fasterxml.jackson.annotation.*;
import java.time.OffsetDateTime;

public class Data {
    private String accessToken;
    private String apiKey;
    private String avatarURL;
    private String broker;
    private String email;
    private String enctoken;
    private String[] exchanges;
    private OffsetDateTime loginTime;
    private Meta meta;
    private String[] orderTypes;
    private String[] products;
    private String publicToken;
    private String refreshToken;
    private String silo;
    private String userID;
    private String userName;
    private String userShortname;
    private String userType;

    @JsonProperty("access_token")
    public String getAccessToken() { return accessToken; }
    @JsonProperty("access_token")
    public void setAccessToken(String value) { this.accessToken = value; }

    @JsonProperty("api_key")
    public String getAPIKey() { return apiKey; }
    @JsonProperty("api_key")
    public void setAPIKey(String value) { this.apiKey = value; }

    @JsonProperty("avatar_url")
    public String getAvatarURL() { return avatarURL; }
    @JsonProperty("avatar_url")
    public void setAvatarURL(String value) { this.avatarURL = value; }

    @JsonProperty("broker")
    public String getBroker() { return broker; }
    @JsonProperty("broker")
    public void setBroker(String value) { this.broker = value; }

    @JsonProperty("email")
    public String getEmail() { return email; }
    @JsonProperty("email")
    public void setEmail(String value) { this.email = value; }

    @JsonProperty("enctoken")
    public String getEnctoken() { return enctoken; }
    @JsonProperty("enctoken")
    public void setEnctoken(String value) { this.enctoken = value; }

    @JsonProperty("exchanges")
    public String[] getExchanges() { return exchanges; }
    @JsonProperty("exchanges")
    public void setExchanges(String[] value) { this.exchanges = value; }

    @JsonProperty("login_time")
    public OffsetDateTime getLoginTime() { return loginTime; }
    @JsonProperty("login_time")
    public void setLoginTime(OffsetDateTime value) { this.loginTime = value; }

    @JsonProperty("meta")
    public Meta getMeta() { return meta; }
    @JsonProperty("meta")
    public void setMeta(Meta value) { this.meta = value; }

    @JsonProperty("order_types")
    public String[] getOrderTypes() { return orderTypes; }
    @JsonProperty("order_types")
    public void setOrderTypes(String[] value) { this.orderTypes = value; }

    @JsonProperty("products")
    public String[] getProducts() { return products; }
    @JsonProperty("products")
    public void setProducts(String[] value) { this.products = value; }

    @JsonProperty("public_token")
    public String getPublicToken() { return publicToken; }
    @JsonProperty("public_token")
    public void setPublicToken(String value) { this.publicToken = value; }

    @JsonProperty("refresh_token")
    public String getRefreshToken() { return refreshToken; }
    @JsonProperty("refresh_token")
    public void setRefreshToken(String value) { this.refreshToken = value; }

    @JsonProperty("silo")
    public String getSilo() { return silo; }
    @JsonProperty("silo")
    public void setSilo(String value) { this.silo = value; }

    @JsonProperty("user_id")
    public String getUserID() { return userID; }
    @JsonProperty("user_id")
    public void setUserID(String value) { this.userID = value; }

    @JsonProperty("user_name")
    public String getUserName() { return userName; }
    @JsonProperty("user_name")
    public void setUserName(String value) { this.userName = value; }

    @JsonProperty("user_shortname")
    public String getUserShortname() { return userShortname; }
    @JsonProperty("user_shortname")
    public void setUserShortname(String value) { this.userShortname = value; }

    @JsonProperty("user_type")
    public String getUserType() { return userType; }
    @JsonProperty("user_type")
    public void setUserType(String value) { this.userType = value; }
}
