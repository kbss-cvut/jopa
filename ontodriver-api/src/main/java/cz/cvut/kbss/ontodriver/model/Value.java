/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    public static final Value<Void> NULL_VALUE = new NullValue();

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
        return value != null ? value.toString() : "";
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
    public static Value<Void> nullValue() {
        return NULL_VALUE;
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
            if (other.value != null)
                return false;
        } else if (!value.equals(other.value))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return value.toString();
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
