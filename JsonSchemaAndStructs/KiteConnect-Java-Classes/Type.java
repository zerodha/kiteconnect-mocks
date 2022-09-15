package io.quicktype;

import java.io.IOException;
import com.fasterxml.jackson.annotation.*;

public enum Type {
    INTEGER, NUMBER, STRING;

    @JsonValue
    public String toValue() {
        switch (this) {
            case INTEGER: return "integer";
            case NUMBER: return "number";
            case STRING: return "string";
        }
        return null;
    }

    @JsonCreator
    public static Type forValue(String value) throws IOException {
        if (value.equals("integer")) return INTEGER;
        if (value.equals("number")) return NUMBER;
        if (value.equals("string")) return STRING;
        throw new IOException("Cannot deserialize Type");
    }
}
