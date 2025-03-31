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

import java.net.MalformedURLException;
import java.net.URL;

/**
 * Converter to {@link java.net.URL}.
 * <p>
 * Only {@code String} values are supported.
 */
public class ToURLConverter implements ConverterWrapper<URL, Object> {

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return String.class.equals(type);
    }

    @Override
    public Object convertToAxiomValue(URL value) {
        return value.toString();
    }

    @Override
    public URL convertToAttribute(Object value) {
        try {
            return new URL(value.toString());
        } catch (MalformedURLException e) {
            throw new UnsupportedTypeTransformationException("Unable to convert value " + value + " to URL.", e);
        }
    }
}
