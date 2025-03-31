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

import static org.junit.jupiter.api.Assertions.*;

class AxiomValueDescriptorTest {

    private static final URI CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
    private static final Assertion ASSERTION = Assertion
            .createPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/propertyOne"), false);

    private AxiomValueDescriptor sut;

    @BeforeEach
    void setUp() {
        sut = new AxiomValueDescriptor(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
    }

    @Test
    void testSetSubjectContext() {
        assertNotNull(sut.getSubjectContexts());
        sut.setSubjectContext(CONTEXT);
        assertEquals(CONTEXT, sut.getSubjectContext());
        assertNotNull(sut.getSubjectContexts());
        assertEquals(Collections.singleton(CONTEXT), sut.getSubjectContexts());
    }

    @Test
    void getSubjectContextsReturnsEmptySetForDefaultContext() {
        assertNotNull(sut.getSubjectContexts());
        assertTrue(sut.getSubjectContexts().isEmpty());
        assertNull(sut.getSubjectContext());
    }

    @Test
    void setAssertionContextSetsContextForAssertion() {
        sut.addAssertion(ASSERTION);
        sut.setAssertionContext(ASSERTION, CONTEXT);
        assertEquals(Collections.singleton(CONTEXT), sut.getAssertionContexts(ASSERTION));
        assertEquals(CONTEXT, sut.getAssertionContext(ASSERTION));
    }

    @Test
    void setAssertionContextThrowsIllegalArgumentWhenDescriptorDoesNotContainAssertion() {
        assertThrows(IllegalArgumentException.class, () -> sut.setAssertionContext(ASSERTION, CONTEXT));
    }

    @Test
    void getAssertionContextsReturnsEmptyCollectionForDefaultContext() {
        sut.addAssertion(ASSERTION);
        sut.setSubjectContext(CONTEXT);
        sut.setAssertionContext(ASSERTION, null);
        assertNotNull(sut.getAssertionContexts(ASSERTION));
        assertTrue(sut.getAssertionContexts(ASSERTION).isEmpty());
        assertNull(sut.getAssertionContext(ASSERTION));
    }

    @Test
    void getAssertionContextsReturnsSubjectContextsWhenNoContextsAreSpecifiedForAssertion() {
        sut.addAssertion(ASSERTION);
        sut.setSubjectContext(CONTEXT);
        assertEquals(Collections.singleton(CONTEXT), sut.getAssertionContexts(ASSERTION));
        assertEquals(CONTEXT, sut.getAssertionContext(ASSERTION));
    }
}
