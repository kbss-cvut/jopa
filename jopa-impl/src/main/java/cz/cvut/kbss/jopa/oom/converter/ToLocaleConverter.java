/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;

import java.util.Locale;

/**
 * Default converter for instances of {@link java.util.Locale}.
 * <p>
 * It stores the values in language tag form as RDF simple literals.
 */
public class ToLocaleConverter implements ConverterWrapper<Locale, Object> {

    @Override
    public Object convertToAxiomValue(Locale value) {
        return new Literal(value.toLanguageTag(), XSD.STRING);
    }

    @Override
    public Locale convertToAttribute(Object value) {
        if (value instanceof Literal literal) {
            return Locale.forLanguageTag(literal.getLexicalForm());
        } else if (value instanceof LangString langString) {
            return Locale.forLanguageTag(langString.getValue());
        }
        return Locale.forLanguageTag(value.toString());
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return String.class.equals(type) || Literal.class.equals(type) || LangString.class.equals(type);
    }
}
