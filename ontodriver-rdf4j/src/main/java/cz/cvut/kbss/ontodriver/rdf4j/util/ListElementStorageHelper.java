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
package cz.cvut.kbss.ontodriver.rdf4j.util;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Value;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Helper class for handling list/container elements storage, especially multilingual strings.
 */
public class ListElementStorageHelper {

    private final ValueConverter valueConverter;

    public ListElementStorageHelper(ValueConverter valueConverter) {this.valueConverter = valueConverter;}

    /**
     * Maps the specified assertion value to RDF4J value(s).
     * <p>
     * If the value is a {@link Translations}, it is mapped to multiple RDF4J values, one for each translation.
     *
     * @param a     Assertion
     * @param value Value to map
     * @return Collection of RDF4J value(s)
     * @throws Rdf4jDriverException If the value conversion fails
     */
    public Collection<Value> toRdf4jValue(Assertion a, Object value) throws Rdf4jDriverException {
        if (value instanceof Translations mls) {
            final List<Value> values = new ArrayList<>(mls.getValue().size());
            for (Map.Entry<String, String> e : mls.getValue().entrySet()) {
                values.add(valueConverter.toRdf4jValue(a, new cz.cvut.kbss.ontodriver.model.Value<>(new LangString(e.getValue(), e.getKey()))));
            }
            return values;
        }
        return List.of(valueConverter.toRdf4jValue(a, new cz.cvut.kbss.ontodriver.model.Value<>(value)));
    }

    /**
     * Extract translations from the specified collection of values.
     *
     * @param values Values assumed to be translations
     * @return Extracted translations
     */
    public static Translations extractTranslations(Collection<Value> values) {
        final Translations mls = new Translations();
        values.forEach(v -> {
            if (!v.isLiteral() || ((Literal) v).getLanguage().isEmpty()) {
                throw new IntegrityConstraintViolatedException("Expected translations, but got " + values);
            }
            final Literal lit = (Literal) v;
            mls.set(lit.getLanguage().get(), lit.getLabel());
        });
        return mls;
    }
}
