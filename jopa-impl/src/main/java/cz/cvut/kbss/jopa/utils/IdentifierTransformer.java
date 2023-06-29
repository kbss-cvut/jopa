/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.utils;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

import static cz.cvut.kbss.jopa.model.PersistenceProperties.IDENTIFIER_TYPES;

public class IdentifierTransformer {

    private static final Map<Class<?>, Function<Object, ?>> TRANSFORMERS = initTransformers();

    private IdentifierTransformer() {
        throw new AssertionError();
    }

    private static Map<Class<?>, Function<Object, ?>> initTransformers() {
        final Map<Class<?>, Function<Object, ?>> m = new HashMap<>(IDENTIFIER_TYPES.size());
        m.put(URI.class, val -> {
            Objects.requireNonNull(val);
            if (val instanceof URI) {
                return val;
            } else {
                return URI.create(val.toString());
            }
        });
        m.put(URL.class, val -> {
            Objects.requireNonNull(val);
            if (val instanceof URL) {
                return val;
            }
            try {
                return new URL(val.toString());
            } catch (MalformedURLException e) {
                throw new IllegalArgumentException("The identifier is not a valid URL.", e);
            }
        });
        m.put(String.class, val -> {
            Objects.requireNonNull(val);
            return val.toString();
        });
        return m;
    }

    /**
     * Transforms the specified value to the target identifier type (if possible).
     *
     * @param value      The value to transform
     * @param targetType Target type
     * @return The transformed value
     * @throws IllegalArgumentException If the target type is not a valid identifier type
     */
    public static Object transformToIdentifier(Object value, Class<?> targetType) {
        if (!isValidIdentifierType(targetType)) {
            throw new IllegalArgumentException(
                    "The specified value " + value + " cannot be transformed to target type " + targetType);
        }
        return TRANSFORMERS.get(targetType).apply(value);
    }

    public static URI valueAsUri(Object value) {
        return (URI) TRANSFORMERS.get(URI.class).apply(value);
    }

    /**
     * Checks whether the specified type is an identifier type supported by JOPA.
     *
     * @param type The type to check
     * @return {@code true} if type is supported identifier type, {@code false} otherwise
     */
    public static boolean isValidIdentifierType(Class<?> type) {
        return type != null && IDENTIFIER_TYPES.contains(type);
    }

    /**
     * Stringifies the specified IRI identifier by enclosing it in &lt; and &gt;.
     *
     * @param identifier Identifier to stringify
     * @return String URI
     */
    public static String stringifyIri(Object identifier) {
        return "<" + identifier + ">";
    }
}
