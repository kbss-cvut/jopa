/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import java.util.Objects;

/**
 * Represents assertion value.
 * <p>
 * This class is a base for both property and class assertion values.
 */
public class Value<T> {

    /**
     * Represents a null value - empty value
     */
    private static final Value<?> NULL_VALUE = new NullValue();

    private final T value;

    private Value() {
        this.value = null;
    }

    public Value(T value) {
        Objects.requireNonNull(value);
        this.value = value;
    }

    /**
     * Gets this value.
     *
     * @return Value of the appropriate type
     */
    public T getValue() {
        return value;
    }

    /**
     * Gets this value as string.
     *
     * @return Value as string
     */
    public String stringValue() {
        return value != null ? value.toString() : null;
    }

    /**
     * Returns a Null object for Value.
     * <p>
     * Since Value requires a non-null value, this method returns a predefined object which represents a null (empty)
     * value.
     *
     * @return Null value
     * @see <a href="http://en.wikipedia.org/wiki/Null_Object_pattern">http://en.wikipedia.org/wiki/Null_Object_pattern</a>
     */
    @SuppressWarnings("unchecked")
    public static <T> Value<T> nullValue() {
        return (Value<T>) NULL_VALUE;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((value == null) ? 0 : value.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Value<?> other = (Value<?>) obj;
        if (value == null) {
            return other.value == null;
        } else return value.equals(other.value);
    }

    @Override
    public String toString() {
        return value != null ? value.toString() : "";
    }

    private static final class NullValue extends Value<Void> {

        private NullValue() {
            super();
        }

        @Override
        public String stringValue() {
            return "null";
        }
    }
}
