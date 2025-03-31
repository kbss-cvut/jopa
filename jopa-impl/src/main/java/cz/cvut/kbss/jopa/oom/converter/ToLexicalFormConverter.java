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

import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;

/**
 * Converts literal lexical form to Java {@code String}.
 * <p>
 * This converter ensures seamless support for lexical forms.
 */
public class ToLexicalFormConverter implements ConverterWrapper<String, Object> {

    public static final ToLexicalFormConverter INSTANCE = new ToLexicalFormConverter();

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        // Anything but a NamedResource (i.e., a literal since anonymous individuals are not supported) can be transformed to lexical form
        return !NamedResource.class.isAssignableFrom(type);
    }

    @Override
    public Object convertToAxiomValue(String value) {
        return value;
    }

    @Override
    public String convertToAttribute(Object value) {
        assert value != null;
        return value instanceof LangString ? ((LangString) value).getValue() : value.toString();
    }
}
