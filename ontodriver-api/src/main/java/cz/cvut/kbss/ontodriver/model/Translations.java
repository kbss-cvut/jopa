/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Value object representing the same string in multiple languages.
 * <p>
 * This is not a general-purpose type, it should be used only in instances where multiple translations of the same
 * string need to be treated as a single value. Its main purpose is to allow support for multilingual strings in
 * referenced lists (e.g., {@link cz.cvut.kbss.ontodriver.Lists#loadReferencedList(ReferencedListDescriptor)}).
 */
public final class Translations implements Serializable {

    private final Map<String, String> value;

    public Translations() {
        this.value = new HashMap<>();
    }

    public Translations(Map<String, String> value) {
        this.value = new HashMap<>(Objects.requireNonNull(value));
    }

    /**
     * Sets translation in the specified language.
     *
     * @param language Language tag value
     * @param value    Translation in the specified language
     */
    public void set(String language, String value) {
        Objects.requireNonNull(value);
        this.value.put(language, value);
    }

    /**
     * Gets translation in the specified language (if present).
     *
     * @param language Language tag
     * @return Translation in the specified language, {@code null} if it is not available
     */
    public String get(String language) {
        return value.get(language);
    }

    /**
     * Gets an unmodifiable view of the internal representation of this multilingual string.
     *
     * @return Unmodifiable map
     */
    public Map<String, String> getValue() {
        return Collections.unmodifiableMap(value);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Translations that)) {
            return false;
        }
        return value.equals(that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return value.toString();
    }
}
