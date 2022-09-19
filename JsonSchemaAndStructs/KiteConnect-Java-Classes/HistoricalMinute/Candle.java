package HistoricalMinute;

import java.io.IOException;
import java.io.IOException;
import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.annotation.*;

@JsonDeserialize(using = Candle.Deserializer.class)
@JsonSerialize(using = Candle.Serializer.class)
public class Candle {
    public Double doubleValue;
    public String stringValue;

    static class Deserializer extends JsonDeserializer<Candle> {
        @Override
        public Candle deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException, JsonProcessingException {
            Candle value = new Candle();
            switch (jsonParser.currentToken()) {
                case VALUE_NUMBER_INT:
                case VALUE_NUMBER_FLOAT:
                    value.doubleValue = jsonParser.readValueAs(Double.class);
                    break;
                case VALUE_STRING:
                    String string = jsonParser.readValueAs(String.class);
                    value.stringValue = string;
                    break;
                default: throw new IOException("Cannot deserialize Candle");
            }
            return value;
        }
    }

    static class Serializer extends JsonSerializer<Candle> {
        @Override
        public void serialize(Candle obj, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
            if (obj.doubleValue != null) {
                jsonGenerator.writeObject(obj.doubleValue);
                return;
            }
            if (obj.stringValue != null) {
                jsonGenerator.writeObject(obj.stringValue);
                return;
            }
            throw new IOException("Candle must not be null");
        }
    }
}
