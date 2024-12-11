/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.util.ValueConverter;
import org.eclipse.rdf4j.model.Value;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

class ReferencedListHelper {

    private final ValueConverter valueConverter;

    ReferencedListHelper(ValueConverter valueConverter) {this.valueConverter = valueConverter;}

    Collection<Value> toRdf4jValue(Assertion a, Object value) throws Rdf4jDriverException {
        if (value instanceof Translations) {
            final Translations mls = (Translations) value;
            final List<Value> values = new ArrayList<>(mls.getValue().size());
            for (Map.Entry<String, String> e : mls.getValue().entrySet()) {
                values.add(valueConverter.toRdf4jValue(a, new cz.cvut.kbss.ontodriver.model.Value<>(new LangString(e.getValue(), e.getKey()))));
            }
            return values;
        }
        return List.of(valueConverter.toRdf4jValue(a, new cz.cvut.kbss.ontodriver.model.Value<>(value)));
    }
}
