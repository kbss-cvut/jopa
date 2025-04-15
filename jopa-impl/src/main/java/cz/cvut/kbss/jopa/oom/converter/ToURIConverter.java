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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.exception.UnsupportedTypeTransformationException;

import java.net.URI;

/**
 * Converter to {@link java.net.URI}.
 * <p>
 * Only {@code String} values are supported.
 */
public class ToURIConverter implements ConverterWrapper<URI, Object> {
    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return String.class.equals(type);
    }

    @Override
    public Object convertToAxiomValue(URI value) {
        return value.toString();
    }

    @Override
    public URI convertToAttribute(Object value) {
        try {
            return URI.create(value.toString());
        } catch (IllegalArgumentException e) {
            throw new UnsupportedTypeTransformationException("Unable to convert value " + value + " to URI.", e);
        }
    }
}
