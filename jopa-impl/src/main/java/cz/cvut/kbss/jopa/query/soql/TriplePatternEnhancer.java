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
package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;

import java.util.List;

/**
 * Enhances triple patterns if necessary.
 * <p>
 * For example, an RDF list enhancer may add triple patterns for getting list elements.
 */
class TriplePatternEnhancer {

    /**
     * Gets (possibly enhanced) triple patterns for the specified subject, predicate, and object.
     *
     * @param subject   Subject position value
     * @param predicate Predicate position value
     * @param object    Object position value
     * @return Triple patterns
     */
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        return List.of(subject + " " + predicate + " " + object + " . ");
    }

    /**
     * Creates the appropriate triple pattern enhancer for the specified attribute.
     *
     * @param attribute Attribute to get enhancer for
     * @return Triple pattern enhancer
     */
    static TriplePatternEnhancer create(FieldSpecification<?, ?> attribute) {
        if (attribute.isMappedAttribute() && attribute.isCollection()) {
            final PluralAttribute<?, ?, ?> pluralAttribute = (PluralAttribute<?, ?, ?>) attribute;
            if (pluralAttribute.isRdfContainer()) {
                return new RdfContainerTriplePatternEnhancer();
            }
            if (pluralAttribute.isRDFCollection()) {
                return new RdfCollectionTriplePatternEnhancer();
            }
            if (pluralAttribute.getCollectionType() == CollectionType.LIST) {
                final ListAttribute<?, ?> listAttribute = (ListAttribute<?, ?>) pluralAttribute;
                if (listAttribute.getSequenceType() == SequenceType.simple) {
                    return new SimpleListTriplePatternEnhancer(listAttribute);
                } else {
                    assert listAttribute.getSequenceType() == SequenceType.referenced;
                    return new ReferencedListTriplePatterEnhancer(listAttribute);
                }
            }
        }

        return new TriplePatternEnhancer();
    }
}
