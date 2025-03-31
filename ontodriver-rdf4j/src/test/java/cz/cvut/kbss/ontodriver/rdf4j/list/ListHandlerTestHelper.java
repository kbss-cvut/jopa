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
package cz.cvut.kbss.ontodriver.rdf4j.list;

import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Vocabulary;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ListHandlerTestHelper {

    static final NamedResource OWNER = NamedResource.create(Generator.generateUri());

    static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";
    static final String NODE_CONTENT_PROPERTY =
            "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContents";

    static List<NamedResource> generateList() {
        return generateList(5);
    }

    static List<NamedResource> generateList(int count) {
        return IntStream.range(0, count)
                        .mapToObj(i -> NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "elem" + i))
                        .collect(Collectors.toList());
    }
}
