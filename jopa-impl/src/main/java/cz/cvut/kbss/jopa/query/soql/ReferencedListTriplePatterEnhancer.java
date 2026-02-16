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

import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

import java.util.List;

public class ReferencedListTriplePatterEnhancer extends TriplePatternEnhancer {

    private final ListAttribute<?, ?> attribute;

    public ReferencedListTriplePatterEnhancer(ListAttribute<?, ?> attribute) {this.attribute = attribute;}

    @Override
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        return List.of(
                subject + " " + predicate + "/(" + IdentifierTransformer.stringifyIri(attribute.getHasNextPropertyIRI()) + "*/" + IdentifierTransformer.stringifyIri(attribute.getHasContentsPropertyIRI()) + ") " + object + " . ",
                "FILTER (!isBlank(" + object + ")) "
        );
    }
}
