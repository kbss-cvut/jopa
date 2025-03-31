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
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Translations;

/**
 * Supports mapping selected value types to multilingual strings.
 * <p>
 * This is not a general-purpose converter, it should be used only by the mapping of referenced lists containing
 * multilingual string values.
 */
public class ToMultilingualStringConverter implements ConverterWrapper<MultilingualString, Object> {

    @Override
    public Object convertToAxiomValue(MultilingualString value) {
        return new Translations(value.getValue());
    }

    @Override
    public MultilingualString convertToAttribute(Object value) {
        final Class<?> type = value.getClass();
        assert supportsAxiomValueType(type);
        if (value instanceof Translations translations) {
            return new MultilingualString(translations.getValue());
        } else if (value instanceof LangString ls) {
            return MultilingualString.create(ls.getValue(), ls.getLanguage().orElse(null));
        } else {
            return MultilingualString.create(value.toString(), null);
        }
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return Translations.class.isAssignableFrom(type)
                || LangString.class.isAssignableFrom(type)
                || String.class.isAssignableFrom(type);
    }
}
