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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.BeforeEach;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

public class SimpleListIteratorTest extends OwlapiListIteratorBase {

    private SimpleListDescriptor descriptor;

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        final OWLNamedIndividual individual = snapshot.getDataFactory().getOWLNamedIndividual(IRI.create(SUBJECT));
        this.testHelper = new SimpleListTestHelper(snapshot, individual);
        this.descriptor = new SimpleListDescriptorImpl(NamedResource.create(SUBJECT), ListTestHelper.HAS_LIST,
                ListTestHelper.HAS_NEXT);
        this.iterator = iterator();
    }

    @Override
    protected OwlapiListIterator<NamedResource> iterator() {
        return new SimpleListIterator(descriptor, snapshot, axiomAdapter);
    }
}
