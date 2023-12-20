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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.mockito.Mock;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;

abstract class ListHandlerTestHelper {

    static final NamedResource OWNER = NamedResource.create(Generator.generateUri());
    static final Assertion HAS_LIST = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    static final Assertion HAS_NEXT = Assertion.createObjectPropertyAssertion(Generator.generateUri(), false);
    static final Resource OWNER_RESOURCE = createResource(OWNER.getIdentifier().toString());
    static final Property HAS_LIST_PROPERTY = createProperty(HAS_LIST.getIdentifier().toString());
    static final Property HAS_NEXT_PROPERTY = createProperty(HAS_NEXT.getIdentifier().toString());

    @Mock
    StorageConnector connectorMock;

    ListTestUtil listUtil;

    public void setUp() {
        this.listUtil = new ListTestUtil(OWNER_RESOURCE, HAS_LIST_PROPERTY, HAS_NEXT_PROPERTY, connectorMock);
    }
}
