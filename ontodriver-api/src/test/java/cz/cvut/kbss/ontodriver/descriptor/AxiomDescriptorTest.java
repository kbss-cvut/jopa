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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;
import java.util.Set;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class AxiomDescriptorTest {

    private static final URI CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
    private static final Assertion ASSERTION = Assertion
            .createPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/propertyOne"), false);

    private AxiomDescriptor sut;

    @BeforeEach
    void setUp() {
        sut = new AxiomDescriptor(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
    }

    @Test
    void testAddSubjectContext() {
        assertNotNull(sut.getSubjectContexts());
        sut.addSubjectContext(CONTEXT);
        assertNotNull(sut.getSubjectContexts());
        assertEquals(Collections.singleton(CONTEXT), sut.getSubjectContexts());
    }

    @Test
    void addSubjectContextResetsContextsWhenNullIsAdded() {
        sut.addSubjectContext(CONTEXT).addSubjectContext(null);
        assertNotNull(sut.getSubjectContexts());
        assertTrue(sut.getSubjectContexts().isEmpty());
    }

    @Test
    void getSubjectContextsReturnsEmptySetForDefaultContext() {
        assertNotNull(sut.getSubjectContexts());
        assertTrue(sut.getSubjectContexts().isEmpty());
    }

    @Test
    void getSubjectContextsReturnsEmptyCollectionForDefaultContext() {
        assertTrue(sut.getSubjectContexts().isEmpty());
    }

    @Test
    void testAddAssertionContexts() {
        sut.addAssertion(ASSERTION);
        sut.addAssertionContext(ASSERTION, CONTEXT);
        assertEquals(Collections.singleton(CONTEXT), sut.getAssertionContexts(ASSERTION));
    }

    @Test
    void descriptorSupportsMultipleSubjectContexts() {
        sut.addSubjectContext(CONTEXT);
        final URI anotherContext = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/contextTwo");
        sut.addSubjectContext(anotherContext);
        final Set<URI> result = sut.getSubjectContexts();
        assertEquals(2, result.size());
        assertThat(result, hasItems(CONTEXT, anotherContext));
    }

    @Test
    void descriptorSupportsMultipleAssertionContexts() {
        sut.addAssertion(ASSERTION);
        sut.addAssertionContext(ASSERTION, CONTEXT);
        final URI anotherContext = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/contextTwo");
        sut.addAssertionContext(ASSERTION, anotherContext);
        final Set<URI> result = sut.getAssertionContexts(ASSERTION);
        assertEquals(2, result.size());
        assertThat(result, hasItems(CONTEXT, anotherContext));
    }

    @Test
    void addAssertionContextThrowsIllegalArgumentWhenDescriptorDoesNotContainAssertion() {
        assertThrows(IllegalArgumentException.class, () -> sut.addAssertionContext(ASSERTION, CONTEXT));
    }

    @Test
    void getAssertionContextsReturnsEmptyCollectionForDefaultContext() {
        sut.addAssertion(ASSERTION);
        sut.addAssertionContext(ASSERTION, CONTEXT).addAssertionContext(ASSERTION, null);
        assertNotNull(sut.getAssertionContexts(ASSERTION));
        assertTrue(sut.getAssertionContexts(ASSERTION).isEmpty());
    }

    @Test
    void getAssertionContextsReturnsSubjectContextsWhenNoContextsAreSpecifiedForAssertion() {
        sut.addAssertion(ASSERTION);
        sut.addSubjectContext(CONTEXT);
        assertEquals(Collections.singleton(CONTEXT), sut.getAssertionContexts(ASSERTION));
    }
}
