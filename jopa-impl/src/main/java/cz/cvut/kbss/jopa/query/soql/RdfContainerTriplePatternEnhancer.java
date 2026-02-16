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

import cz.cvut.kbss.jopa.vocabulary.RDF;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Enhances triple patterns for a RDF container attribute.
 */
class RdfContainerTriplePatternEnhancer extends TriplePatternEnhancer {

    // Remember containers already visited to ensure hasElement variable name is unique
    private final Map<Long, Integer> visitedContainers = new HashMap<>();

    @Override
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        // Make variable names dependent on predicate to avoid collisions. This is not bulletproof but should be good enough.
        // Cannot use just hashCode because it may be negative, making the variable name invalid in SPARQL
        final long predicateHash = Math.abs(predicate.hashCode());  // Widen to long as abs of hashcode may be negative
        final String containerVariable = "?rdfContainer" + predicateHash;
        String hasElementVariable = "?hasElement" + predicateHash;
        if (visitedContainers.containsKey(predicateHash)) {
            int count = visitedContainers.get(predicateHash);
            hasElementVariable = "?hasElement" + predicateHash + "_" + count++;
            visitedContainers.put(predicateHash, count);
        } else {
            visitedContainers.put(predicateHash, 1);
        }
        return List.of(
                subject + " " + predicate + " " + containerVariable + " . ",
                containerVariable + " " + hasElementVariable + " " + object + " . ",
                "FILTER (STRSTARTS(STR(" + hasElementVariable + "), \"" + RDF.NAMESPACE + "_\")) "
        );
    }
}
