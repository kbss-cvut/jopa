package cz.cvut.kbss.ontodriver.model;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

/**
 * Represents a string value with a (optional) language tag.
 */
public class LangString implements Serializable {

    private final String value;

    private final String language;

    public LangString(String value) {
        this.value = Objects.requireNonNull(value);
        this.language = null;
    }

    public LangString(String value, String language) {
        this.value = Objects.requireNonNull(value);
        this.language = language;
    }

    /**
     * Gets the lexical value of this string.
     */
    public String getValue() {
        return value;
    }

    /**
     * Gets the language tag (if present).
     */
    public Optional<String> getLanguage() {
        return Optional.ofNullable(language);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof LangString)) {
            return false;
        }
        LangString that = (LangString) o;
        return value.equals(that.value) && Objects.equals(language, that.language);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value, language);
    }

    @Override
    public String toString() {
        return language != null ? "\"" + value + "\"@" + language : "\"" + value + '"';
    }
}
