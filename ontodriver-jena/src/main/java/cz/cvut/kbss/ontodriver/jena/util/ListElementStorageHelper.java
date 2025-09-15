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
package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.RDFNode;

import java.util.Collection;
import java.util.Objects;
import java.util.stream.Stream;

public class ListElementStorageHelper {

    private ListElementStorageHelper() {
        throw new AssertionError();
    }

    /**
     * Maps the specified value to a stream of RDF nodes.
     * <p>
     * The method returns a stream because the value may be mapped to multiple nodes.
     *
     * @param value             Value to map
     * @param propertyAssertion Property assertion whose values are mapped to RDF nodes
     * @return Stream of RDF nodes
     */
    public static Stream<RDFNode> toRdfNodes(Object value, Assertion propertyAssertion) {
        Objects.requireNonNull(value);
        Objects.requireNonNull(propertyAssertion);

        if (value instanceof Translations mls) {
            return mls.getValue().entrySet().stream()
                      .map(e -> JenaUtils.valueToRdfNode(propertyAssertion, new Value<>(new LangString(e.getValue(), e.getKey()))));
        } else {
            return Stream.of(JenaUtils.valueToRdfNode(propertyAssertion, new Value<>(value)));
        }
    }

    /**
     * Extracts translations from the specified collection of RDF nodes.
     * <p>
     * That is, assuming the values are literals with language tags, extract them into a {@link Translations} object.
     *
     * @param values Values to extract
     * @return Translations
     * @throws IntegrityConstraintViolatedException If the values are not translations
     */
    public static Translations extractTranslations(Collection<RDFNode> values) {
        final Translations mls = new Translations();
        values.forEach(n -> {
            if (!n.isLiteral() || n.asLiteral().getLanguage().isEmpty()) {
                throw new IntegrityConstraintViolatedException("Expected translations, but got " + values);
            }
            final Literal lit = n.asLiteral();
            assert lit.getLanguage() != null;
            mls.set(lit.getLanguage(), lit.getString());
        });
        return mls;
    }
}
