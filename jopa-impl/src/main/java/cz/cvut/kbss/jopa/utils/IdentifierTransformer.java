/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

/**
 * @author ledvima1
 */
class IdentifierTransformer {

    /**
     * Transforms the specified value to the target type (if possible).
     *
     * @param value      The value to transform
     * @param targetType Target type
     * @return The transformed value
     */
    public Object transformToType(Object value, Class<?> targetType) {
        if (targetType.isAssignableFrom(value.getClass())) {
            return value;
        }
        if (URI.class.isAssignableFrom(targetType)) {
            return URI.create(value.toString());
        } else if (URL.class.isAssignableFrom(targetType)) {
            return valueAsUrl(value);
        } else if (String.class.isAssignableFrom(targetType)) {
            return value.toString();
        }
        throw new IllegalArgumentException(
                "The specified value " + value + " cannot be transformed to target type " + targetType);
    }

    URI valueAsUri(Object value) {
        if (value instanceof URI) {
            return (URI) value;
        } else {
            return URI.create(value.toString());
        }
    }

    private URL valueAsUrl(Object value) {
        try {
            return new URL(value.toString());
        } catch (MalformedURLException e) {
            throw new IllegalArgumentException("The identifier is not a valid URL.", e);
        }
    }
}
