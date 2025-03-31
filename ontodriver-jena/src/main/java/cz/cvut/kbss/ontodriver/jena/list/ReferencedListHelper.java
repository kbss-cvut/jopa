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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.RDFNode;

import java.util.stream.Stream;

class ReferencedListHelper {

    private ReferencedListHelper() {
        throw new AssertionError();
    }

    static Stream<RDFNode> toRdfNodes(Object value, Assertion nodeContentAssertion) {
        if (value instanceof Translations mls) {
            return mls.getValue().entrySet().stream()
                      .map(e -> JenaUtils.valueToRdfNode(nodeContentAssertion, new Value<>(new LangString(e.getValue(), e.getKey()))));
        } else {
            return Stream.of(JenaUtils.valueToRdfNode(nodeContentAssertion, new Value<>(value)));
        }
    }
}
