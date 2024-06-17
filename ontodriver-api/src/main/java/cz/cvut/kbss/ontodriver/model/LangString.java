/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
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

    /**
     * Explicit no-arg constructor allowing deserialization from JSON.
     */
    private LangString() {
        this.value = null;
        this.language = null;
    }

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
     *
     * @return Lexical value
     */
    public String getValue() {
        return value;
    }

    /**
     * Gets the language tag (if present).
     *
     * @return Optional language tag value
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
        return Objects.equals(value, that.value) && Objects.equals(language, that.language);
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
