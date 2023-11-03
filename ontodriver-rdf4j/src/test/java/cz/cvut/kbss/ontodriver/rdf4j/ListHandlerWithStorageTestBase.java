/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.connector.Connector;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.environment.TestRepositoryProvider;
import org.junit.jupiter.api.AfterEach;

import java.util.Collection;
import java.util.Iterator;

import static org.junit.jupiter.api.Assertions.assertEquals;

abstract class ListHandlerWithStorageTestBase<D extends ListDescriptor, V extends ListValueDescriptor> {

    static final NamedResource OWNER = NamedResource.create(Generator.generateUri());

    static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";

    final TestRepositoryProvider repositoryProvider = new TestRepositoryProvider();

    protected Connector connector;

    protected ListHandler<D, V> handler;

    @AfterEach
    public void tearDown() throws Exception {
        connector.close();
        repositoryProvider.close();
    }

    void verifyListContent(Collection<Axiom<NamedResource>> expected, Collection<Axiom<NamedResource>> actual) {
        assertEquals(expected.size(), actual.size());
        // This is more explicit on failure than just containsAll
        final Iterator<Axiom<NamedResource>> itExp = expected.iterator();
        final Iterator<Axiom<NamedResource>> itAct = actual.iterator();
        while (itExp.hasNext()) {
            assertEquals(itExp.next(), itAct.next());
        }
    }

    void updateAndCheck(V descriptor) throws Exception {
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        handler.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList((D) descriptor));
    }

    abstract Collection<Axiom<NamedResource>> generateAxiomsForList(V listDescriptor);
}
