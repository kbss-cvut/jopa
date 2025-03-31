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

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;

/**
 * Allows to convert values between arbitrary types.
 * <p>
 * The main intended use is for annotation property mapping to attributes of type {@link Object}, which can hold both
 * literal values and references to other individuals. In that case, the loaded instance is a {@link NamedResource} and
 * needs to be transformed to a {@link java.net.URI} to prevent the internal OntoDriver API from leaking into the
 * application.
 * <p>
 * Similarly, OntoDriver API's {@link LangString} is transformed to {@link MultilingualString}.
 * <p>
 * In all other cases, the values will be just returned without any conversion.
 */
public class ObjectConverter implements ConverterWrapper<Object, Object> {

    private final boolean preferMultilingualString;

    public ObjectConverter() {
        this.preferMultilingualString = false;
    }

    public ObjectConverter(boolean preferMultilingualString) {
        this.preferMultilingualString = preferMultilingualString;
    }

    @Override
    public Object convertToAxiomValue(Object value) {
        if (IdentifierTransformer.isValidIdentifierType(value.getClass()) && !(value instanceof String)) {
            return NamedResource.create(IdentifierTransformer.valueAsUri(value));
        }
        return value;
    }

    @Override
    public Object convertToAttribute(Object value) {
        if (value instanceof NamedResource) {
            return ((NamedResource) value).getIdentifier();
        } else if (value instanceof LangString ls) {
            if (preferMultilingualString) {
                final MultilingualString ms = new MultilingualString();
                ms.set(ls.getLanguage().orElse(null), ls.getValue());
                return ms;
            } else {
                return ls.getValue();
            }
        }
        return value;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
    }

    public boolean doesPreferMultilingualString() {
        return preferMultilingualString;
    }
}
